{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
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
import Control.Monad.Maybe
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.Async

import Debug.Trace

macSpaceDim  = 100
switchNum    = 16
maxTimeout   = 100
payloadLen   = 128
statsDelay   = 500000

pktInQLen     = 10000
pktInQTimeout = 0.01

whenJustM :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJustM (Just v) m  = m v
whenJustM Nothing  _  = return ()

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

-- FIXME: same pmap for different threads --- this is a bug!

data PktStatsEvent =   PktInSent
                     | PktOutRecv NominalDiffTime
                     | PktLost    Int

type PacketQ = IntMap.IntMap UTCTime

empyPacketQ :: PacketQ
empyPacketQ = IntMap.empty

onSend :: TVar PacketQ -> TBMChan PktStatsEvent -> OfpMessage -> IO ()
onSend q s (OfpMessage _ (OfpPacketInReply (OfpPacketIn bid _ _ _))) = do
  now <- getCurrentTime
  atomically $ do
    modifyTVar q (IntMap.insert (fromIntegral bid) now)
    writeTBMChan s PktInSent
  sweepQ now

  where
    sweepQ now = atomically $ do
      pq <- readTVar q
      when (IntMap.size pq > pktInQLen) $ do
      let (lost, rest) = IntMap.partition ((>pktInQTimeout).toRational.diffUTCTime now) pq
      writeTVar q $! rest
      writeTBMChan s $! PktLost (IntMap.size lost)

onSend _ _ _ = return ()

onReceive :: TVar PacketQ -> TBMChan PktStatsEvent -> OfpMessage -> IO ()
onReceive q s (OfpMessage _ (OfpPacketOut (OfpPacketOutData bid pid))) = do
  now <- getCurrentTime
  pq <- (atomically.readTVar) q

  whenJustM (IntMap.lookup ibid pq) $ \dt -> atomically $ do
    writeTBMChan s (PktOutRecv (now `diffUTCTime` dt))
    modifyTVar q $ IntMap.delete ibid

  where ibid = fromIntegral bid

onReceive _ _ (OfpMessage h _)  = do
  return ()

data PktStats = PktStats { pktStatsSentTotal :: !Int
                         , pktStatsRecvTotal :: !Int
                         , pktStatsLostTotal :: !Int
                         , pktStatsConnLost  :: !Int
                         }

emptyStats :: PktStats
emptyStats = PktStats 0 0 0 0

updateStats :: TBMChan PktStatsEvent -> TVar PktStats -> IO ()
updateStats q s = forever $ (atomically (readTBMChan q)) >>= maybe skip withEvent
  where
    withEvent (PktOutRecv _) =
      atomically $ modifyTVar s $! \st -> st { pktStatsRecvTotal = succ (pktStatsRecvTotal st) }

    withEvent (PktLost n) =
      atomically $ modifyTVar s $! \st -> st { pktStatsLostTotal = succ (pktStatsLostTotal st) }

    withEvent pktInSent =
      atomically $ modifyTVar s $! \st -> st { pktStatsSentTotal = succ (pktStatsSentTotal st) }

    skip = return ()

type HCState = (Int,Int,UTCTime)    
newtype IOStat a = IOStat { runIOStat :: StateT HCState IO a
                          } deriving(Monad, MonadIO, MonadState HCState)

printStat tst = do
  hSetBuffering stdout NoBuffering
  initTime <- getCurrentTime
  flip runStateT (0,0,initTime) $ runIOStat $ forever $ do
      (psent, precv, ptime) <- get
      now <- liftIO $ getCurrentTime
      let !dt   = toRational (now `diffUTCTime` ptime)
      when ( fromRational dt > 0 ) $ do
        st  <- liftIO $ atomically $ readTVar tst
        let !sent = pktStatsSentTotal st
        let !recv = pktStatsRecvTotal st
        let !sps  = floor ((toRational (sent - psent)) / dt) :: Int
        let !rps  = floor ((toRational (recv - precv)) / dt) :: Int
        let !lost = pktStatsLostTotal st
        let !err  = pktStatsConnLost st
        put (sent, recv, now)
        let s = printf "sent: %6d (%5d p/sec) recv: %6d (%5d p/sec) lost: %5d err: %5d  \r" sent sps recv rps lost err
        liftIO $ hPutStr stdout s >> threadDelay statsDelay
  return ()

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
  stats  <- newTVarIO emptyStats
  statsQ <- newTBMChanIO 10000

  fakeSw <- forM [1..switchNum] $ \i -> do
    let ip = (fromIntegral i) .|. (0x10 `shiftL` 24)
    rnd <- newStdGen
    macs <- liftM S.toList (randomSet (48*macSpaceDim) S.empty)
    let !(fake'@(FakeSwitch sw _ _ _ _),_) = makeSwitch (defaultSwGen i ip rnd) 48 macs [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]
    pktQ <- newTVarIO empyPacketQ
    return $ fake' { onSendMessage = Just (onSend pktQ statsQ), onRecvMessage = Just (onReceive pktQ statsQ) }

  workers <- forM fakeSw $ \fake -> async $ forever $ do
        (async (ofpClient pktGenTest fake (BS8.pack host) (read port))) >>= wait
        atomically $ modifyTVar stats ( \s -> s { pktStatsConnLost = succ (pktStatsConnLost s) } )
        threadDelay 1000000
  
  async (printStat stats)
  async (updateStats statsQ stats)

  waitAnyCatch workers

  putStrLn ""
  putStrLn "done"

