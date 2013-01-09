{-# LANGUAGE OverloadedStrings, BangPatterns #-}
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
import Text.Printf
import Data.Maybe
import Data.List (intersperse, concat, unfoldr)
import qualified Data.IntMap as IntMap 

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

mcPrefix = ((.|.)(0x00163e `shiftL` 24)).((.&.)0xFFFFFF)

macSpaceDim = 100

testTCP dstMac srcMac = do
  srcIp  <- randomIO :: IO IPv4Addr
  dstIp  <- randomIO :: IO IPv4Addr
  srcP   <- randomIO :: IO Word16
  dstP   <- randomIO :: IO Word16
  wss    <- randomIO :: IO Int 
  flags  <- return [ACK]
  cargo  <- replicateM 128 randomIO :: IO [Word8]
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
pktGenTest fk chan = forever $ do
    tid <- randomIO :: IO Word32
    delay <- liftM (`mod` maxTimeout) randomIO :: IO Int
    bid <- liftM ((`mod` nbuf)) randomIO     :: IO Word32
    pid <- liftM ((+1).(`mod` (nports-1)))  randomIO :: IO Int 
    n1  <- randomIO :: IO Int
    n2  <- randomIO :: IO Int
    dct <- macSpaceDict
    msp <- macSpace
    let !srcMac' = IntMap.lookup pid dct >>= choice n1
    let !dstMac' = choice n2 msp
    case (srcMac', dstMac') of 
      (Just srcMac, Just dstMac) -> do tid <- randomIO :: IO Word32
                                       pl  <- liftM (encodePutM.putEthernetFrame) (testTCP dstMac srcMac)
                                       atomically $ writeTBMChan chan $! (tcpTestPkt fk tid bid (fromIntegral pid) pl)
      _                          -> putStrLn "FUCKUP"
    threadDelay delay

  where nbuf = (fromIntegral.ofp_n_buffers.switchFeatures) fk
        nports = (fromIntegral.length.ofp_ports.switchFeatures) fk
        inports = fromIntegral nports :: Int
        maxTimeout = 10000
        macSpace  = replicateM macSpaceDim (liftM mcPrefix randomIO)
        macSpace' = macSpace >>= return.((takeWhile (not.null)).(unfoldr (Just.(splitAt mpp))))
        macSpaceDict :: IO (IntMap.IntMap [MACAddr])
        macSpaceDict = macSpace' >>= return . IntMap.fromList . (zip [1..inports-1])
        mpp = macSpaceDim `div` inports - 1
        choice n l  = Just $ l !! (n `mod` length l)
        choice n [] = Nothing

tcpTestPkt fk tid bid pid pl = OfpMessage hdr (OfpPacketInReply  pktIn)
  where hdr   = header openflow_1_0 tid OFPT_PACKET_IN
        pktIn = OfpPacketIn { ofp_pkt_in_buffer_id = bid
                            , ofp_pkt_in_in_port   = pid 
                            , ofp_pkt_in_reason    = OFPR_NO_MATCH
                            , ofp_pkt_in_data      = pl
                            }
        sw  = switchFeatures fk





data PktStats = PktStats { pktInSent :: !Int
                         , pktOutRcv :: !Int
                         , pmap      :: !(IntMap.IntMap Int)
                         }

onSend :: TVar PktStats -> OfpMessage -> IO ()
onSend s (OfpMessage _ (OfpPacketInReply (OfpPacketIn bid _ _ _))) = atomically $ do
  st <- readTVar s
  writeTVar s $! st { pktInSent = succ (pktInSent st)
                    , pmap = (IntMap.insert (fromIntegral bid) 0 (pmap st)) -- TODO: truncate map on overflow
                    }

onSend _ _ = return ()

-- FIXME: stack space overflow during stats. update

onReceive :: TVar PktStats -> OfpMessage -> IO ()
onReceive s (OfpMessage _ (OfpPacketOut (OfpPacketOutData bid pid))) = atomically $ do
    st <- readTVar s
    writeTVar s $! st { pktOutRcv = succ (pktOutRcv st)
                      , pmap = IntMap.delete (fromIntegral bid) (pmap st) -- TODO: truncate map on overflow
                      }

onReceive _ (OfpMessage h _)  = do
  return ()

-- TODO: calculate PktIn/s statistics
printStat tst = do
  hSetBuffering stdout NoBuffering
  initTime <- getCurrentTime  
  flip runStateT (0,initTime) $ forever $ do
    st <- lift $ atomically $! readTVar tst
    (np,t0) <- get
    t1 <- lift $ getCurrentTime
    let pktIS  = pktInSent st
    let dt     = toRational (t1 `diffUTCTime` t0)
    let pktISS = floor ((toRational (pktIS - np)) / dt) :: Int
    let pktOR  = pktOutRcv st
    let qL     = (IntMap.size . pmap) st
    let stats  = printf "Stats:  pktIn: %6d pktIn/s: %6d pktout: %6d qlen: %6d     \r" pktIS  pktISS pktOR qL
    put (pktIS, t1)
    lift $ hPutStr stdout stats
    lift $ threadDelay 500000

main :: IO ()
main = do
  (host:port:_) <- getArgs
  stats <- newTVarIO (PktStats 0 0 IntMap.empty)
  workers <- forM [1..100] $ \i -> do
    let ip = (fromIntegral i) .|. (0x10 `shiftL` 24)
    rnd <- newStdGen
    let !(fake'@(FakeSwitch sw _ _ _),_) = makeSwitch (defaultSwGen i ip rnd) 48 [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]
    let fake = fake' { onSendMessage = Just (onSend stats), onRecvMessage = Just (onReceive stats) }
    async $ ofpClient pktGenTest fake (BS8.pack host) (read port)
  ps <- async (printStat stats)
  mapM_ wait (workers)
  putStrLn "done"

