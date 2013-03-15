{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Main where 

import Network.Openflow.Types 
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
import qualified System.Random.Mersenne as MR
import System.IO
import System.Environment (getArgs)
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Maybe
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.Async

import System.IO.Error

import qualified Statistics.Sample as S

import Debug.Trace


whenJustM :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJustM (Just v) m  = m v
whenJustM Nothing  _  = return ()

testTCP params dstMac srcMac = do
  srcIp  <- randomIO :: IO IPv4Addr
  dstIp  <- randomIO :: IO IPv4Addr
  srcP   <- randomIO :: IO Word16
  dstP   <- randomIO :: IO Word16
  wss    <- randomIO :: IO Int 
  let flags  = [ACK]
--  cargo  <- replicateM payloadLen randomIO :: IO [Word8]
  let cargo = replicate (payloadLen params) 0
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

type PacketQ = IntMap.IntMap UTCTime

empyPacketQ :: PacketQ
empyPacketQ = IntMap.empty

pktGenTest :: Parameters -> TVar PacketQ -> FakeSwitch -> TBMChan OfpMessage -> IO ()
pktGenTest params q fk chan  = forever $ do
    pq  <- atomically $ readTVar q
    tid <- MR.randomIO :: IO Word32

--    putStrLn "AAAA"  
   -- mtgen <- MR.newMTGen Nothing 
   -- putStrLn "BBBB"  

    rands <-   MR.getStdGen >>= MR.randoms --return [1..100] -- MR.randoms mtgen
    let bid = fromIntegral $ head $ filter (not.flip IntMap.member pq) rands -- TODO try to put all in one expression.
    pid <- liftM ((+2).(`mod` (nports-1)))     MR.randomIO :: IO Int
    pidDst <- liftM ((+2).(`mod` (nports-1)))  MR.randomIO :: IO Int

    when (pid /= pidDst ) $ do
      n1  <- MR.randomIO :: IO Int
      n2  <- MR.randomIO :: IO Int
      let dct = macSpace fk
      let !srcMac' = IntMap.lookup pid    dct >>= choice n1
      let !dstMac' = IntMap.lookup pidDst dct >>= choice n2
      case (srcMac', dstMac') of 
        (Just srcMac, Just dstMac) -> do pl  <- liftM (encodePutM.putEthernetFrame) (testTCP params dstMac srcMac )
                                         atomically $ writeTBMChan chan $! tcpTestPkt fk tid bid (fromIntegral pid) pl
        _                          -> return ()

    delay <- liftM ((+ ((maxTimeout params) `div` 2)).(`mod` ((maxTimeout params) `div` 2))) MR.randomIO :: IO Int
 --   print delay
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

onSend :: Parameters -> TVar PacketQ -> TVar PktStats -> OfpMessage -> IO ()
onSend params q s (OfpMessage _ (OfpPacketInReply (OfpPacketIn bid _ _ _))) = do
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

onReceive _ _ (OfpMessage h _)  = 
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

updateLog :: Parameters -> TBMChan LogEntry -> [TVar PktStats] -> IO ()
updateLog params chan tst = do
  initTime <- getCurrentTime
  flip runStateT (0,0,initTime) $ runIOStat $ forever $ do
      (psent, precv, ptime) <- get
      now <- liftIO getCurrentTime
      let !dt   = toRational (now `diffUTCTime` ptime)
      when ( fromRational dt > 0 ) $ do
        stats <- liftIO $ mapM (atomically.readTVar) tst
        let !st = sumStat stats
        let !rtts = BV.fromList $ (map (fromRational.toRational).mapMaybe pktStatsRoundtripTime) stats :: BV.Vector Double
        let !mean = S.mean rtts * 1000 :: Double
        let !sent = pktStatsSentTotal st
        let !recv = pktStatsRecvTotal st
        let !sps  = floor (toRational (sent - psent) / dt) :: Int
        let !rps  = floor (toRational (recv - precv) / dt) :: Int
        let !lost = pktStatsLostTotal st
        let !err  = pktStatsConnLost st
        put (sent, recv, now)
        liftIO $ atomically $ writeTBMChan chan   LogEntry { logTimestamp     = now `diffUTCTime` initTime
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


writeLog :: Parameters -> TBMChan LogEntry -> IO ()
writeLog params chan = whenJustM (logFileName params) $ \fn -> withFile fn WriteMode $ \h ->
  forever $ do
    hSetBuffering h NoBuffering -- If buffering on, tail won't be pused to file
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
    threadDelay $ samplingPeriod params `div` 2

displayStats :: Parameters -> TBMChan LogEntry -> IO ()
displayStats params chan = do
  hSetBuffering stdout NoBuffering
  initTime <- getCurrentTime
  let reader = if isNothing $ logFileName params
                    then readTBMChan
                    else peekTBMChan
    
  forever $ do
    logm <- atomically $ reader chan
    whenJustM logm $ \log -> do
--      putStrLn "Log OK"
      let !ts   = (fromRational.toRational.logTimestamp) log :: Double
      let !sent = logSent log
      let !recv = logRecv log
      let !sps  = logSendPerSecond log
      let !rps  = logRecvPerSecond log
      let !lost = logLost log
      let !err  = logErrors log
      let !mean = logMeanRtt log
      let !s = printf "t: %4.3fs sent: %6d (%5d p/sec) recv: %6d (%5d p/sec) mean rtt: %4.2fms lost: %3d err: %3d  \r" ts sent sps recv rps mean lost err
      putStr s
      threadDelay $ statsDelay params

randomSet :: Word64 -> S.Set MACAddr -> IO (S.Set MACAddr)

--randomSet n s | S.size s == n = return s

randomSet n s = do
  i <- randomIO :: IO Word8
  let macList = [1..fromIntegral n] :: [MACAddr]
  return $ foldl (insertSet $ fromIntegral i) s macList 
  where insertSet i c v =                          --Insert i*v value to map
            if S.member (i*v) c                    --If this value is already exists in map
                then insertSet (i+1) c v           --Try to ad (i+1)*v value to map
                else S.insert (mcPrefix (i*v)) c   --If all ok, add i*v value to map
  --if not (S.member i s)
    --then randomSet n (S.insert i s)
    --else randomSet n s

toTryMain :: IO ()
toTryMain = do
  params <- getParameters   -- Read parameters from cmd Args and config file
                            -- All wariables like macSpaceDim, switchNum, etc. was replased
                            -- by expression like (macSpaceDim params) (switchNum params) etc.
 -- print $ macSpaceDim params
  
  stats   <- newTVarIO emptyStats
  statsQ  <- newTBMChanIO 10000
  testLog <- newTBMChanIO 10000

--  mtgen <- MR.newMTGen Nothing

  fakeSw <- forM [1..switchNum params] $ \i -> do
    let ip = fromIntegral i .|. (0x10 `shiftL` 24)
    rnd <- newStdGen
    macs <- liftM S.toList (randomSet (fromIntegral (portNum params) * macSpaceDim params+1) S.empty)
--    print $ length macs
    return $ fst $ makeSwitch (defaultSwGen i ip rnd) (portNum params) macs [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]

    
  w <- forM fakeSw $ \fake' -> do
        pktQ <- newTVarIO empyPacketQ
        stat <- newTVarIO emptyStats
        let fake = fake' { onSendMessage = Just (onSend params pktQ stat), onRecvMessage = Just (onReceive pktQ stat) }
        w <- async $ forever $ do
          async (ofpClient (pktGenTest params pktQ) fake (BS8.pack (host params)) (read (port params))) >>= wait
          atomically $ modifyTVar stats (\s -> s { pktStatsConnLost = succ (pktStatsConnLost s) })
          threadDelay 1000000
        return (w,stat)
  
  let workers = map fst w        

  misc  <- mapM async [ updateLog params testLog (stats : map snd w) 
                      , writeLog params testLog
                      , displayStats params testLog
                      ]

  async $ threadDelay (testDuration params + 350000) >> mapM_ cancel (misc ++ workers)

  mapM_ waitCatch (workers ++ misc)

  putStrLn ""
  putStrLn "done"

main :: IO ()
main = 
    toTryMain --`catch` parseHandler
