{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
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
-- Module with configurations reader
-- Read parameters from cmd args and/or configfile
-- First reads configfile then some parametes can be replased in cmd args
-- Parameters, don't meeted in config file or in cmd args setted as default.
import HCProbe.Configurator

import qualified Network.Openflow.StrictPut as SP

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
import Data.Typeable
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as BV
import Data.List (intersperse, concat, unfoldr)
import qualified Data.List as L
import qualified Data.IntMap as IntMap
import qualified Data.Map as M 

import System.Random
import System.IO
import System.Environment (getArgs)
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Error
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.Async

import System.IO.Error

import qualified Statistics.Sample as S

import Debug.Trace

ethernetFrameMaxSize = 2048

whenJustM :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJustM (Just v) m  = m v
whenJustM Nothing  _  = return ()

testTCP dstMac srcMac params = do
  srcIp  <- randomIO :: IO IPv4Addr
  dstIp  <- randomIO :: IO IPv4Addr
  srcP   <- randomIO :: IO Word16
  dstP   <- randomIO :: IO Word16
  wss    <- randomIO :: IO Int 
  return $! TestPacketTCP { dstMAC = dstMac
                          , srcMAC = srcMac
                          , srcIP  = srcIp
                          , dstIP  = dstIp
                          , dstPort = dstP
                          , srcPort = srcP
                          , testWSS = Just wss
                          , testFlags = tcpFlagsOf [ACK]
                          , testPayloadLen = (payloadLen params) 
                          , testSeqNo = Nothing
                          , testAckNo = Nothing
                          , testIpID = Nothing
                          }

type PacketQ = IntMap.IntMap UTCTime

empyPacketQ :: PacketQ
empyPacketQ = IntMap.empty

-- FIXME: improve pktIn/s performance
-- FIXME: improve mac space generation performace

pktGenTest :: TVar PacketQ -> Parameters -> FakeSwitch -> TBMChan OfpMessage -> IO ()
pktGenTest q params fk chan  = forever $ do
    pq  <- atomically $ readTVar q
    tid <- randomIO :: IO Word32
    bid <- liftM (fromIntegral.head.filter (not.flip IntMap.member pq).randoms) newStdGen
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
                                         pl  <- liftM (SP.runPutToByteString ethernetFrameMaxSize.putEthernetFrame) (testTCP dstMac srcMac params)
                                         atomically $ writeTBMChan chan $! (tcpTestPkt fk tid bid (fromIntegral pid) pl)
        _                          -> return ()

      delay <- liftM (`mod` maxTimeout params) randomIO :: IO Int
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

data PktStats = PktStats { pktStatsSentTotal :: !Int
                         , pktStatsRecvTotal :: !Int
                         , pktStatsLostTotal :: !Int
                         , pktStatsConnLost  :: !Int
                         , pktStatsRoundtripTime :: !(Maybe NominalDiffTime)
                         }

emptyStats :: PktStats
emptyStats = PktStats 0 0 0 0 Nothing

onSend :: TVar PacketQ -> TVar PktStats -> Parameters -> OfpMessage -> IO ()
onSend q s params (OfpMessage _ (OfpPacketInReply (OfpPacketIn bid _ _ _))) = do
  now <- getCurrentTime
  atomically $ do
    modifyTVar q (IntMap.insert (fromIntegral bid) now)
    modifyTVar s (\st -> st { pktStatsSentTotal = succ (pktStatsSentTotal st)
                            })
  sweepQ now

  where
    sweepQ now = atomically $ do
      pq <- readTVar q
      when (IntMap.size pq > pktInQLen params) $ do
      let rationalTimeout = toRational (pktInQTimeout params)
      let (lost, rest) = IntMap.partition ((>rationalTimeout).toRational.diffUTCTime now) pq
      writeTVar q $! rest
      modifyTVar s (\st -> st { pktStatsLostTotal = succ (pktStatsLostTotal st)
                              })

onSend _ _ _ _ = return ()

onReceive :: TVar PacketQ -> TVar PktStats -> OfpMessage -> IO ()
onReceive q s (OfpMessage _ (OfpPacketOut (OfpPacketOutData bid pid))) = do
  now <- getCurrentTime
  pq <- (atomically.readTVar) q

  whenJustM (IntMap.lookup ibid pq) $ \dt -> atomically $ do
    modifyTVar s (\st -> st { pktStatsSentTotal = succ (pktStatsSentTotal st)
                            , pktStatsRoundtripTime = Just( now `diffUTCTime` dt )
                            })
    modifyTVar q $ IntMap.delete ibid

  where ibid = fromIntegral bid

onReceive _ _ (OfpMessage h _)  = do
  return ()


type HCState = (Int,Int,UTCTime)    
newtype IOStat a = IOStat { runIOStat :: StateT HCState IO a
                          } deriving(Monad, MonadIO, MonadState HCState)

data LogEntry = LogEntry { logTimestamp     :: !NominalDiffTime
                         , logSendPerSecond :: !Int
                         , logRecvPerSecond :: !Int
                         , logSent          :: !Int
                         , logRecv          :: !Int
                         , logMeanRtt       :: !Double
                         , logErrors        :: !Int
                         , logLost          :: !Int
                         }

updateLog :: TBMChan LogEntry -> [TVar PktStats] -> Parameters -> IO ()
updateLog chan tst params = do
  initTime <- getCurrentTime
  flip runStateT (0,0,initTime) $ runIOStat $ forever $ do
      (psent, precv, ptime) <- get
      now <- liftIO $ getCurrentTime
      let !dt   = toRational (now `diffUTCTime` ptime)
      when ( fromRational dt > 0 ) $ do
        stats <- liftIO $ mapM (atomically.readTVar) tst
        let st = sumStat stats
        let rtts = BV.fromList $ (map (fromRational.toRational).catMaybes.map pktStatsRoundtripTime) stats :: BV.Vector Double
        let mean = S.mean rtts * 1000 :: Double
        let !sent = pktStatsSentTotal st
        let !recv = pktStatsRecvTotal st
        let !sps  = floor ((toRational (sent - psent)) / dt) :: Int
        let !rps  = floor ((toRational (recv - precv)) / dt) :: Int
        let !lost = pktStatsLostTotal st
        let !err  = pktStatsConnLost st
        put (sent, recv, now)
        liftIO $ atomically $ writeTBMChan chan $ LogEntry { logTimestamp     = now `diffUTCTime` initTime
                                                           , logSendPerSecond = sps
                                                           , logRecvPerSecond = rps
                                                           , logSent          = sent
                                                           , logRecv          = recv
                                                           , logMeanRtt       = mean
                                                           , logErrors        = err
                                                           , logLost          = lost
                                                           }
        liftIO $ threadDelay (samplingPeriod params)

  return ()

  where
    sumStat = foldr sumStep emptyStats
    sumStep s st' = st' { pktStatsSentTotal = sent s + sent st'
                        , pktStatsRecvTotal = recv s + sent st'
                        , pktStatsLostTotal = lost s + lost st'
                        , pktStatsConnLost  = clost s + clost st'
                        }
    sent  = pktStatsSentTotal
    recv  = pktStatsRecvTotal
    lost  = pktStatsLostTotal
    clost = pktStatsConnLost


writeLog :: TBMChan LogEntry -> Parameters -> IO ()
writeLog chan params = whenJustM (logFileName params) $ \fn -> withFile fn WriteMode $ \h -> do
  forever $ do
    log' <- atomically $ readTBMChan chan
    whenJustM log' $ \log -> do
      let !ts   = (fromRational.toRational.logTimestamp) log :: Double
      let !sent = logSent log
      let !recv = logRecv log
      let !sps  = logSendPerSecond log
      let !rps  = logRecvPerSecond log
      let !lost = logLost log
      let !err  = logErrors log
      let !mean = logMeanRtt log
      let !s = printf "%6.4f\t%6d\t%6d\t%6d\t%6d\t%6.4f\t%6d\t%6d" ts sent sps recv rps mean lost err
      hPutStrLn h s
    threadDelay $ (samplingPeriod params) `div` 2

displayStats :: TBMChan LogEntry -> Parameters -> IO ()
displayStats chan params = do
  hSetBuffering stdout NoBuffering
  initTime <- getCurrentTime
  forever $ do
    logm <- atomically $ peekTBMChan chan
    whenJustM logm $ \log -> do
      let !ts   = (fromRational.toRational.logTimestamp) log :: Double
      let !sent = logSent log
      let !recv = logRecv log
      let !sps  = logSendPerSecond log
      let !rps  = logRecvPerSecond log
      let !lost = logLost log
      let !err  = logErrors log
      let !mean = logMeanRtt log
      let s = printf "t: %4.3fs sent: %6d (%5d p/sec) recv: %6d (%5d p/sec) mean rtt: %4.2fms lost: %3d err: %3d  \r" ts sent sps recv rps mean lost err
      hPutStr stdout s
      threadDelay $ statsDelay params

randomSet :: Int -> S.Set MACAddr -> IO (S.Set MACAddr)

randomSet n s | S.size s == n = return s

randomSet n s = do
  i <- liftM mcPrefix randomIO
  if not (S.member i s)
    then randomSet n (S.insert i s)
    else randomSet n s

toTryMain :: IO ()
toTryMain = do
  params <- getParameters   -- Read parameters from cmd Args and config file
                            -- All wariables like macSpaceDim, switchNum, etc. was replased
                            -- by expression like (macSpaceDim params) (switchNum params) etc.
  print $ logFileName params
  
  stats   <- newTVarIO emptyStats
  statsQ  <- newTBMChanIO 10000
  testLog <- newTBMChanIO 10000

  fakeSw <- forM [1..(switchNum params)] $ \i -> do
    let ip = (fromIntegral i) .|. (0x10 `shiftL` 24)
    rnd <- newStdGen
    macs <- liftM S.toList (randomSet (48*(macSpaceDim params)) S.empty)
    return $ fst $ makeSwitch (defaultSwGen i ip rnd) 48 macs [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]

  w <- forM fakeSw $ \fake' -> do
        pktQ <- newTVarIO empyPacketQ
        stat <- newTVarIO emptyStats
        let fake = fake' { onSendMessage = Just (onSend pktQ stat params), onRecvMessage = Just (onReceive pktQ stat) }
        w <- async $ forever $ do
          (async (ofpClient (pktGenTest pktQ params) fake (BS8.pack (host params)) (read (port params)))) >>= wait
          atomically $ modifyTVar stats (\s -> s { pktStatsConnLost = succ (pktStatsConnLost s) })
          threadDelay 1000000
        return (w,stat)
  
  let workers = map fst w        

  misc  <- mapM async [ updateLog testLog (stats : map snd w) params
                      , writeLog testLog params
                      , displayStats testLog params
                      ]

  async $ threadDelay (testDuration params + 350000) >> mapM_ cancel (misc ++ workers)

  mapM_ waitCatch (workers ++ misc)

  putStrLn ""
  putStrLn "done"

main :: IO ()
main = do
    toTryMain --`catch` parseHandler
