{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Main where 

import Network.Openflow.Types 
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Ethernet.IPv4
import Network.Openflow.Messages


import HCProbe.FakeSwitch
import HCProbe.TCP
import HCProbe.Ethernet
-- Module with configurations reader
-- Read parameters from cmd args and/or configfile
-- First reads configfile then some parametes can be replased in cmd args
-- Parameters, don't meeted in config file or in cmd args setted as default.
import HCProbe.Configurator

import qualified Network.Openflow.StrictPut as SP

import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Data.Bits
import Data.Time
import qualified Data.Set as S
import Text.Printf
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as BV
import qualified Data.IntMap as IntMap

import qualified System.Random as R
import {-qualified-} System.Random.Mersenne as MR
import System.IO
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

-- import System.IO.Error

import qualified Statistics.Sample as S

-- import Debug.Trace

ethernetFrameMaxSize :: Integer
ethernetFrameMaxSize = 2048

whenJustM :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJustM (Just v) m  = m v
whenJustM Nothing  _  = return ()

testTCPs :: Parameters
         -> IO (MACAddr -> MACAddr -> IPv4Addr -> IPv4Addr -> TestPacketTCP)
testTCPs params = do
  srcP   <- randomIO :: IO Word16
  dstP   <- randomIO :: IO Word16
  wss    <- randomIO :: IO Int 
  return $! \dstMac srcMac srcIp dstIp ->
            TestPacketTCP { dstMAC = dstMac
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

testTCP :: Parameters -> MACAddr -> MACAddr -> IO TestPacketTCP
testTCP params dstMac srcMac = do
  srcIp  <- randomIO :: IO IPv4Addr
  dstIp  <- randomIO :: IO IPv4Addr
  srcP   <- randomIO :: IO Word16
  dstP   <- randomIO :: IO Word16
  wss    <- randomIO :: IO Int 
  -- let flags  = [ACK]
--  cargo  <- replicateM payloadLen randomIO :: IO [Word8]
  -- let cargo = replicate (payloadLen params) 0
  return $! TestPacketTCP { dstMAC = dstMac
                          , srcMAC = srcMac
                          , srcIP  = srcIp
                          , dstIP  = dstIp
                          , dstPort = dstP
                          , srcPort = srcP
                          , testWSS = Just wss
                          , testFlags = tcpFlagsOf [ACK]
                          , testPayloadLen = (payloadLen params)
                          , testAckNo = Nothing
                          , testSeqNo = Nothing
                          , testIpID = Nothing
                          }

type PacketQ = IntMap.IntMap UTCTime

type  TVarL a  = TVar (Int, TVar a)

empyPacketQ :: PacketQ
empyPacketQ = IntMap.empty

pktGenTest :: (MACAddr -> MACAddr -> IPv4Addr -> IPv4Addr -> TestPacketTCP) -> Parameters -> TVarL PacketQ -> (OfpMessage -> IO ()) -> FakeSwitch -> IO ()
pktGenTest s params q stat fk@(FakeSwitch _ _ _ _ _ (qOut,qIn)) = do
    ls <- MR.randoms =<< MR.getStdGen
    let go (l1:l2:l3:l4:l5:l6:l7:l8:lss) (bid:bs) n mBuffer = do
            let pid = l1 `mod` (nports-1) + 2
                pidDst = l2 `mod` (nports-1) + 2
            if (pid == pidDst)
                then go lss (bid:bs) n mBuffer
                else do
                    pq  <- readTVarIO . snd =<< readTVarIO q
                    if IntMap.member (fromIntegral bid) pq
                         then go lss bs n mBuffer
                         else do
                              let dct = macSpace fk
                              let !srcMac' = choice l3 =<< IntMap.lookup pid dct
                              let !dstMac' = choice l4 =<< IntMap.lookup pidDst dct
                              buf <- case mBuffer of
                                Just b -> return b
                                _      -> atomically $ readTQueue qIn
                              buf' <- case (srcMac', dstMac') of
                                (Just srcMac, Just dstMac) -> 
                                    let pl = putEthernetFrame (EthFrame dstMac srcMac (putIPv4Pkt (s dstMac srcMac (fromIntegral l7) (fromIntegral l8))))
                                        msg= (tcpTestPkt (fromIntegral l5) bid (fromIntegral pid) pl)
                                    in do stat msg
                                          SP.runPutToBuffer buf (putMessage msg)
                                _                          -> return buf

                              if n >= bucketSize params
                                then do
                                  let delay = ((+ ((maxTimeout params) `div` 2)).(`mod` (maxTimeout params `div` 2))) l6
                                  atomically $ writeTQueue qOut buf'
                                  threadDelay delay
                                  go lss bs 0 Nothing
                                else
                                  go lss bs (n+1) (Just buf')
        go _ _ _ _ = error "impossible"
    go ls (map (`mod` maxBuffers) [1..]) 0 Nothing
  where -- nbuf = (fromIntegral.ofp_n_buffers.switchFeatures) fk
        nports = (fromIntegral.length.ofp_switch_features_ports.switchFeatures) fk
        choice n l | V.null l  = Nothing
                   | otherwise = Just $ l `V.unsafeIndex` (n `mod` V.length l)

tcpTestPkt :: Word32 -> Word32 -> Word16 -> SP.PutM () -> OfpMessage
tcpTestPkt tid bid pid pl = OfpMessage hdr (OfpPacketInReply  pktIn)
  where hdr   = header openflow_1_0 tid OFPT_PACKET_IN
        pktIn = OfpPacketIn { ofp_pkt_in_buffer_id = bid
                            , ofp_pkt_in_in_port   = pid 
                            , ofp_pkt_in_reason    = OFPR_NO_MATCH
                            , ofp_pkt_in_data      = pl
                            }

data PktStats = PktStats { pktStatsSentTotal :: !Int
                         , pktStatsRecvTotal :: !Int
                         , pktStatsLostTotal :: !Int
                         , pktStatsConnLost  :: !Int
                         , pktStatsRoundtripTime :: !(Maybe NominalDiffTime)
                         }

emptyStats :: PktStats
emptyStats = PktStats 0 0 0 0 Nothing

onSend :: Parameters -> TVarL PacketQ -> TVar PktStats -> OfpMessage -> IO ()
onSend params q s (OfpMessage _ (OfpPacketInReply (OfpPacketIn bid _ _ _))) = do
  now <- getCurrentTime
  atomically $ do
    (l,s') <- readTVar q
    modifyTVar s' (IntMap.insert (fromIntegral bid) now)
    writeTVar q (l+1,s')
  atomically $ modifyTVar s (\st -> st { pktStatsSentTotal = succ (pktStatsSentTotal st) })
  sweepQ now

  where
    sweepQ now = atomically $ do
      (l,g) <- readTVar q
      when (l > pktInQLen params) $ do
        pq <- readTVar g
        let rationalTimeout = toRational (pktInQTimeout params)
        let (_lost, rest) = IntMap.partition ((>rationalTimeout).toRational.diffUTCTime now) pq
        writeTVar g $! rest
        modifyTVar s (\st -> st { pktStatsLostTotal = succ (pktStatsLostTotal st)
                                  })
        writeTVar q (IntMap.size rest,g)

onSend _ _ _ _ = return ()
{-# INLINE onSend #-}

onReceive :: TVarL PacketQ -> TVar PktStats -> OfpMessage -> IO ()
onReceive q s (OfpMessage _ (OfpPacketOut (OfpPacketOutData bid _pid))) = receivePacket q s bid
onReceive q s (OfpMessage _ (OfpFlowMod OfpFlowModData{ofp_flow_mod_buffer_id=bid})) = receivePacket q s bid
onReceive _ _ (OfpMessage _h _) = return ()
{-# INLINE onReceive #-}

receivePacket :: TVarL PacketQ -> TVar PktStats -> Word32 -> IO ()
receivePacket q s bid = do
  now <- getCurrentTime
  pq  <- readTVarIO . snd =<< readTVarIO q

  whenJustM (IntMap.lookup ibid pq) $ \dt -> do
    atomically $ 
      modifyTVar s (\st -> st { pktStatsRecvTotal = succ (pktStatsRecvTotal st)
                              , pktStatsRoundtripTime = Just $ fromMaybe 0 (pktStatsRoundtripTime st) +
                                                        ( now `diffUTCTime` dt )
                              })
    atomically $ do
      (l,pq') <- readTVar q
      modifyTVar pq' $ IntMap.delete ibid
      writeTVar q (l-1,pq')

  where ibid = fromIntegral bid
{-# INLINE receivePacket #-}


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

updateLog :: Parameters -> TChan LogEntry -> [TVar PktStats] -> IO ()
updateLog params chan tst = do
  initTime <- getCurrentTime
  _ <- flip runStateT (0,0,initTime) $ runIOStat $ forever $ do
      (psent, precv, ptime) <- get
      now <- liftIO getCurrentTime
      let !dt   = toRational (now `diffUTCTime` ptime)
      when ( fromRational dt > (0::Double) ) $ do
        stats <- liftIO $ mapM readTVarIO tst
        let !st = sumStat stats
        let meanRtt s@PktStats{pktStatsRoundtripTime=Just rtt} =
                Just (rtt / (fromRational . toRational . pktStatsRecvTotal $ s))
            meanRtt _ = Nothing
        let !rtts = BV.fromList $ (map (fromRational.toRational).mapMaybe meanRtt) stats :: BV.Vector Double
        let !mean = S.mean rtts * 1000 :: Double
        let !sent' = pktStatsSentTotal st
        let !recv' = pktStatsRecvTotal st
        let !sps  = floor (toRational (sent' - psent) / dt) :: Int
        let !rps  = floor (toRational (recv' - precv) / dt) :: Int
        let !lost' = pktStatsLostTotal st
        let !err  = pktStatsConnLost st
        put (sent', recv', now)
        liftIO $ atomically $ writeTChan chan   LogEntry { logTimestamp     = now `diffUTCTime` initTime
                                                         , logSendPerSecond = sps
                                                         , logRecvPerSecond = rps
                                                         , logSent          = sent'
                                                         , logRecv          = recv'
                                                         , logMeanRtt       = mean
                                                         , logErrors        = err
                                                         , logLost          = lost'
                                                         }
        liftIO $ threadDelay (samplingPeriod params)

  return ()

  where
    sumStat = foldr sumStep emptyStats
    sumStep s st' = st' { pktStatsSentTotal = sent s + sent st'
                        , pktStatsRecvTotal = recv s + recv st'
                        , pktStatsLostTotal = lost s + lost st'
                        , pktStatsConnLost  = clost s + clost st'
                        }
    sent  = pktStatsSentTotal
    recv  = pktStatsRecvTotal
    lost  = pktStatsLostTotal
    clost = pktStatsConnLost


writeLog :: Parameters -> TChan LogEntry -> IO ()
writeLog params chan = whenJustM (logFileName params) $ \fn -> withFile fn WriteMode $ \h ->
  forever $ do
    hSetBuffering h NoBuffering -- If buffering on, tail won't be pused to file
    log' <- atomically $ readTChan chan
    let !ts   = (fromRational.toRational.logTimestamp) log' :: Double
        !sent = logSent log'
        !recv = logRecv log'
        !sps  = logSendPerSecond log'
        !rps  = logRecvPerSecond log'
        !lost = logLost log'
        !err  = logErrors log'
        !mean = logMeanRtt log'
        !s = printf "%6.4f\t%6d\t%6d\t%6d\t%6d\t%6.4f\t%6d\t%6d" ts sent sps recv rps mean lost err
    hPutStrLn h s
    threadDelay $ samplingPeriod params `div` 2

displayStats :: Parameters -> TChan LogEntry -> IO ()
displayStats params chan = do
  hSetBuffering stdout NoBuffering
  forever $ do
    log' <- atomically $ let k = do x <- readTChan chan
                                    l <- isEmptyTChan chan 
                                    if l then return x
                                         else k
                        in k
                                
--      putStrLn "Log OK"
    let !ts   = (fromRational.toRational.logTimestamp) log' :: Double
    let !sent = logSent log'
    let !recv = logRecv log'
    let !sps  = logSendPerSecond log'
    let !rps  = logRecvPerSecond log'
    let !lost = logLost log'
    let !err  = logErrors log'
    let !mean = logMeanRtt log'
    putStr $ printf "t: %4.3fs sent: %6d (%5d p/sec) recv: %6d (%5d p/sec) mean rtt: %4.2fms lost: %3d err: %3d  \r" ts sent sps recv rps mean lost err
    threadDelay $ statsDelay params

randomSet :: Word64 -> S.Set MACAddr -> IO (S.Set MACAddr)
randomSet n s = do
  i <- randomIO :: IO Word8
  let macList = [1..fromIntegral n] :: [MACAddr]
  return $ foldl (insertSet $ fromIntegral i) s macList 
  where insertSet i c v =                          --Insert i*v value to map
            if S.member (i*v) c                    --If this value is already exists in map
                then insertSet (i+1) c v           --Try to ad (i+1)*v value to map
                else S.insert (mcPrefix (i*v)) c   --If all ok, add i*v value to map

toTryMain :: IO ()
toTryMain = do
  params <- getParameters   -- Read parameters from cmd Args and config file
                            -- All wariables like macSpaceDim, switchNum, etc. was replased
                            -- by expression like (macSpaceDim params) (switchNum params) etc.
  
  stats    <- newTVarIO emptyStats
  logsIn   <- newBroadcastTChanIO
  -- statsQ   <- newTBMChanIO 10000
  testLog  <- atomically $ dupTChan logsIn 
  wr <-    if (isJust (logFileName params)) 
              then do ch <- atomically $ dupTChan logsIn
                      return [writeLog params ch]
              else return []

  fakeSw <- forM [1..switchNum params] $ \i -> do
    let ip = fromIntegral i .|. (0x10 `shiftL` 24)
    rnd <- R.newStdGen
    macs <- liftM S.toList (randomSet (fromIntegral (portNum params) * macSpaceDim params+1) S.empty)
    fst <$> makeSwitch (defaultSwGen i ip rnd) (portNum params) macs [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]

    
  w <- forM fakeSw $ \fake' -> do
        pktQ <- newTVarIO . (,) 0 =<< newTVarIO empyPacketQ
        stat <- newTVarIO emptyStats
        let fake = fake' { onSendMessage = onSend params pktQ stat, onRecvMessage = onReceive pktQ stat }
        w <- async $ forever $ do
          
          bs <-  testTCPs params
          wait =<< async (ofpClient (pktGenTest bs params pktQ (onSend params pktQ stat)) fake (BS8.pack (host params)) (read (port params)))
          atomically $ modifyTVar stat (\s -> s { pktStatsConnLost = succ (pktStatsConnLost s) })
          threadDelay 1000000
        return (w,stat)
  
  let workers = map fst w        


  misc  <- mapM async ([ updateLog params logsIn (stats : map snd w) 
                       , displayStats params testLog
                       ] ++ wr)

  _ <- async $ threadDelay (testDuration params + 350000) >> mapM_ cancel (misc ++ workers) -- ??? why async

  mapM_ waitCatch (workers ++ misc)

  putStrLn ""
  putStrLn "done"

main :: IO ()
main = 
    toTryMain --`catch` parseHandler
