{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Misc
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.ARP
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Messages
import Network.Openflow.Misc
import HCProbe.FakeSwitch
import HCProbe.TCP

import Data.Binary.Put ( runPut )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Data.Bits
import Data.Time
import qualified Data.Set as S
import Text.Printf
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Data.List (intersperse, concat, unfoldr)
import qualified Data.IntMap as IntMap
import qualified Data.Map as M 

import System.Random
import System.IO
import System.Environment (getArgs)
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.Async

import Debug.Trace

macSpaceDim = 100
switchNum   = 32
maxTimeout  = 1000
payloadLen  = 64
statsDelay  = 500000
pmapThreshold = 1000

testTCP dstMac srcMac = do
  srcIp  <- randomIO :: IO IPv4Addr
  dstIp  <- randomIO :: IO IPv4Addr
  srcP   <- randomIO :: IO Word16
  dstP   <- randomIO :: IO Word16
  wss    <- randomIO :: IO Int 
  flags  <- return [ACK]
--  cargo  <- replicateM payloadLen randomIO :: IO [Word8]
  let cargo = replicate payloadLen 0
  return $! TestPacketTCP { dstMAC = dstMac
                          , srcMAC = srcMac
                          , srcIP  = srcIp
                          , dstIP  = dstIp
                          , dstPort = dstP
                          , srcPort = srcP
                          , testWSS = Just wss
                          , testFlags = Just flags
                          , payLoad = BS.pack cargo
                          , testSeqNo = Nothing
                          , testAckNo = Nothing
                          , testIpID = Nothing
                          }

-- FIXME: improve pktIn/s performance, move mac space to FakeSwitch
pktGenTest :: FakeSwitch -> TBMChan OfpMessage -> IO ()
pktGenTest fk chan = do
    forever $ do
    tid <- randomIO :: IO Word32
    bid <- liftM ((`mod` nbuf))                randomIO :: IO Word32
    pid <- liftM ((+2).(`mod` (nports-1)))     randomIO :: IO Int 
    pidDst <- liftM ((+2).(`mod` (nports-1)))  randomIO :: IO Int 

    when (pid /= pidDst ) $ do
      n1  <- randomIO :: IO Int
      n2  <- randomIO :: IO Int
      let dct = macSpace fk
      let !srcMac' = IntMap.lookup pid    dct >>= choice n1
      let !dstMac' = IntMap.lookup pidDst dct >>= choice n2
      case (srcMac', dstMac') of 
        (Just srcMac, Just dstMac) -> do tid <- randomIO :: IO Word32
                                         pl  <- liftM (encodePutM.putEthernetFrame) (testTCP dstMac srcMac)
                                         atomically $ writeTBMChan chan $! (tcpTestPkt fk tid bid (fromIntegral pid) pl)
        _                          -> putStrLn "FUCKUP" -- FIXME: {L} add valid error handling

      delay <- liftM (`mod` maxTimeout) randomIO :: IO Int
      threadDelay delay

  where nbuf = (fromIntegral.ofp_n_buffers.switchFeatures) fk
        nports = (fromIntegral.length.ofp_ports.switchFeatures) fk
        inports = fromIntegral nports :: Int
        choice n l | V.null l  = Nothing
                   | otherwise = Just $ l `V.unsafeIndex` (n `mod` V.length l)

tcpTestPkt fk tid bid pid pl = OfpMessage hdr (OfpPacketInReply  pktIn)
  where hdr   = header openflow_1_0 tid OFPT_PACKET_IN
        pktIn = OfpPacketIn { ofp_pkt_in_buffer_id = bid
                            , ofp_pkt_in_in_port   = pid 
                            , ofp_pkt_in_reason    = OFPR_NO_MATCH
                            , ofp_pkt_in_data      = pl
                            }
        sw  = switchFeatures fk

data PktStats = PktStats { pktInSent   :: !Int
                         , pktOutRcv   :: !Int
                         , pktLost     :: !Int
                         , pmap        :: !(IntMap.IntMap Int)
                         , liveThreads :: !Int
                         }

emptyStats = PktStats 0 0 0 IntMap.empty 0

onSend :: TVar PktStats -> OfpMessage -> IO ()
onSend s (OfpMessage _ (OfpPacketInReply (OfpPacketIn bid _ _ _))) = atomically $ do
  st <- readTVar s
  writeTVar s $! st { pktInSent = succ (pktInSent st)
                    , pmap = (IntMap.insert (fromIntegral bid) 0 (pmap st))
                    }

onSend _ _ = return ()

onReceive :: TVar PktStats -> OfpMessage -> IO ()
onReceive s (OfpMessage _ (OfpPacketOut (OfpPacketOutData bid pid))) = atomically $ do
    st <- readTVar s
    when (IntMap.member (fromIntegral bid) (pmap st) ) $ do
      writeTVar s $! trunc $ st { pktOutRcv = succ (pktOutRcv st)
                                , pmap = IntMap.delete (fromIntegral bid) (pmap st)
                                }
    where trunc s = if pmapSz > pmapThreshold
                    then s { pmap = IntMap.empty, pktLost = (pktLost s) + pmapSz }
                    else s
            where pmapSz = IntMap.size (pmap s)

onReceive _ (OfpMessage h _)  = do
  return ()

printStat tst = do
  hSetBuffering stdout NoBuffering
  initTime <- getCurrentTime  
  flip runStateT (0,0,initTime) $ forever $ do
    st <- lift $ atomically $! readTVar tst
    (np,op,t0) <- get
    t1 <- lift $ getCurrentTime
    let pktIS  = pktInSent st
    let dt     = toRational (t1 `diffUTCTime` t0)
    let pktISS = floor ((toRational (pktIS - np)) / dt) :: Int
    let pktOR  = pktOutRcv st
    let pktORS = floor ((toRational (pktOR - op))/ dt) :: Int
    let qL     = (IntMap.size . pmap) st
    let tds    = liveThreads st
    let pLost  = pktLost st
    let stats  = printf "Stats:  pktIn: %6d pktIn/s: %6d pktOut: %5d pktOut/s: %5d pktLost: %5d qlen: %6d tds: %4d  \r" pktIS  pktISS pktOR pktORS pLost qL tds
    put (pktIS, pktOR, t1)
    lift $ hPutStr stdout stats
    lift $ threadDelay statsDelay

randomSet :: Int -> S.Set MACAddr -> IO (S.Set MACAddr)

randomSet n s | S.size s == n = return s

randomSet n s = do
  i <- liftM mcPrefix randomIO
  if not (S.member i s)
    then randomSet n (S.insert i s)
    else randomSet n s

main :: IO ()
main = do
  (host:port:_) <- getArgs
  stats <- newTVarIO emptyStats

  fakeSw <- forM [1..switchNum] $ \i -> do
    let ip = (fromIntegral i) .|. (0x10 `shiftL` 24)
    rnd <- newStdGen
    macs <- liftM S.toList (randomSet (48*macSpaceDim) S.empty)
    let !(fake'@(FakeSwitch sw _ _ _ _),_) = makeSwitch (defaultSwGen i ip rnd) 48 macs [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]
    return $ fake' { onSendMessage = Just (onSend stats), onRecvMessage = Just (onReceive stats) }

  ps <- async (printStat stats)

  workers <- forM fakeSw $ \fake -> async $ forever $ do
        atomically $ modifyTVar stats ( \s -> s { liveThreads = succ (liveThreads s) } )
        (async (ofpClient pktGenTest fake (BS8.pack host) (read port))) >>= wait
        atomically $ modifyTVar stats ( \s -> s { liveThreads = pred (liveThreads s) } )
        threadDelay 1000000

  waitAnyCatch workers

  putStrLn ""
  putStrLn "done"

