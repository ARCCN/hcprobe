{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, CPP #-}

module HCProbe.EDSL.Handlers
    ( OfpPredicate
    , predicateHandler
    , PktStats(..)
    , StatEntry(..)
    , initPacketStats
    , setStatsHandler
    , statsSend
    , statsSendOFPPacketIn
    , getStats
    , assembleStats
    ) where

import qualified Data.IntMap as IM
import Data.IORef
import Data.Time.Clock
import Data.Word
import qualified Data.Vector as BV
import qualified Data.Set as S
import Data.Maybe

import Network.Openflow.Types
import Network.Openflow.Ethernet
import Network.Openflow.Messages
import Network.Openflow.StrictPut
import HCProbe.FakeSwitch
import HCProbe.FakeSwitch.Processing
import HCProbe.EDSL

import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad.State.Lazy

import System.IO
import Text.Printf
import qualified Statistics.Sample as S

type OfpPredicate = OfpType -> Bool

predicateHandler :: Num a
                => OfpPredicate
                -> IORef a
                -> (OfpType, OfpMessage)
                -> IO (OfpType, OfpMessage)
predicateHandler pred ref (t,m) = do
    when (pred t) $
        modifyIORef' ref (\n->n+1)
    return (t,m)

type  TVarL a  = TVar (Int, TVar a)

data PktStats = PktStats { pktStatsSentTotal :: !Int
                         , pktStatsRecvTotal :: !Int
                         , pktStatsLostTotal :: !Int
                         , pktStatsConnLost  :: !Int
                         , pktStatsRoundtripTimes :: ![NominalDiffTime]
                         } deriving Show

data StatEntry = StatEntry { statSentTotal     :: !Int
                           , statRecvTotal     :: !Int
                           , statLostTotal     :: !Int
                           , statBid           :: !Word32
                           , statRoundtripTime :: !NominalDiffTime
                           } deriving Show

data StatsEntity = StatsEntity { packetQ :: TVarL (IM.IntMap UTCTime)
                               , pktStats   :: TVar PktStats
                               , pktInQLen      :: Int
                               , pktInQTimeout  :: Float
                               }
initPacketStats :: (MonadIO m)
                => Int
                -> Float
                -> m StatsEntity
initPacketStats len to = do
    im <- liftIO $ newTVarIO IM.empty
    pQ <- liftIO $ newTVarIO (0,im)
    st <- liftIO $ newTVarIO (PktStats 0 0 0 0 [])
    return $ StatsEntity pQ st len to

setStatsHandler :: StatsEntity
                -> (StatEntry -> IO ())
                -> FakeSwitchM ()
setStatsHandler pS eH = setUserHandler $ statsHandler pS eH

statsHandler :: StatsEntity
             -> (StatEntry -> IO ())
             -> (OfpType, OfpMessage)
             -> IO (OfpType, OfpMessage)
statsHandler (StatsEntity q s _ _) entryHandler
             m@(_, (OfpMessage _ (OfpPacketOut (OfpPacketOutData bid _pid)))) = do
  now <- getCurrentTime
  pq  <- readTVarIO . snd =<< readTVarIO q
  whenJustM (IM.lookup ibid pq) $ \dt -> do
    entry <- atomically $ do
      st <- readTVar s
      let sent = pktStatsSentTotal st
          recvd = succ (pktStatsRecvTotal st)
          lost = pktStatsLostTotal st
          rtt = now `diffUTCTime` dt
          st' = st { pktStatsRecvTotal = recvd
                   , pktStatsRoundtripTimes = rtt : (pktStatsRoundtripTimes st)
                   }
      writeTVar s st'
      (l,pq') <- readTVar q
      modifyTVar pq' $ IM.delete ibid
      writeTVar q (l-1,pq')
      return (StatEntry sent recvd lost bid rtt)
    entryHandler entry
  return m
  where ibid = fromIntegral bid
statsHandler _ _ m = return m

statsOnSend :: (MonadIO m)
            => StatsEntity
            -> OfpMessage
            -> m ()
statsOnSend (StatsEntity q s len to)
            ( OfpMessage _ (OfpPacketInReply (OfpPacketIn bid _ _ _))) = liftIO $ do
  now <- getCurrentTime
  atomically $ do
    (l,s') <- readTVar q
    modifyTVar s' (IM.insert (fromIntegral bid) now)
    writeTVar q (l+1,s')
  atomically $ modifyTVar s (\st -> st { pktStatsSentTotal = succ (pktStatsSentTotal st) })
  sweepQ now

  where
    sweepQ now = atomically $ do
      (l,g) <- readTVar q
      when (l > len) $ do
        pq <- readTVar g
        let rationalTimeout = toRational to
        let (_lost, rest) = IM.partition ((>rationalTimeout).toRational.diffUTCTime now) pq
        writeTVar g $! rest
        modifyTVar s (\st -> st { pktStatsLostTotal = succ (pktStatsLostTotal st)
                                  })
        writeTVar q (IM.size rest,g)
statsOnSend _ _ = return ()

whenJustM :: (Monad m) => (Maybe a) -> (a -> m b) -> m ()
whenJustM Nothing _ = return ()
whenJustM (Just a) m = do
    m a;
    return()

statsSend :: StatsEntity
          -> OfpMessage
          -> FakeSwitchM ()
statsSend q m = do
    statsOnSend q m
    send m

statsSendOFPPacketIn :: StatsEntity
                     -> Word16
                     -> PutM ()
                     -> FakeSwitchM Word32
statsSendOFPPacketIn q pid pl = do
    bid <- nextBID
    xid <- nextXID
    let m = OfpMessage (header openflow_1_0 xid OFPT_PACKET_IN)
                (OfpPacketInReply (OfpPacketIn bid pid OFPR_NO_MATCH pl))
    statsOnSend q m
    send m
    return bid

getStats :: (MonadIO m)
         => StatsEntity
         -> m PktStats
getStats (StatsEntity _ s _ _) =
    liftIO $ readTVarIO s

assembleStats :: (MonadIO m)
              => [StatsEntity]
              -> m PktStats
assembleStats lSE = liftIO $ do
  lPS <- sequence $ map getStats lSE
  return $ sumStat lPS

sumStat :: [PktStats] -> PktStats
sumStat lPS = foldl1 addStats lPS
  where
    addStats (PktStats s r l cl rtt ) (PktStats as ar al acl artt) =
      PktStats (s+as) (r+ar) (l+al) (cl+acl) (rtt ++ artt)

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

runStatsLogging :: (MonadIO m)
                => [StatsEntity]
                -> Int
                -> Maybe String
                -> Bool
                -> m ()
runStatsLogging lSE sP logFile display = liftIO $ do
  tChan <- newTChanIO
  let wl = case logFile of Nothing -> []
                           Just file -> [writeLog sP file tChan]
      ds = if display then (displayStats sP tChan):wl
                      else wl
  when (not $ null ds) $ do
     mapM async $ (updateLog sP tChan lSE):ds
     return ()

updateLog :: Int -> TChan LogEntry -> [StatsEntity] -> IO ()
updateLog samplingPeriod chan lSE = do
  let tst = map pktStats lSE
  initTime <- getCurrentTime
  _ <- flip runStateT (0,0,initTime) $ runIOStat $ forever $ do
      (psent, precv, ptime) <- get
      now <- liftIO getCurrentTime
      let !dt   = toRational (now `diffUTCTime` ptime)
      when ( fromRational dt > (0::Double) ) $ do
        stats <- liftIO $ mapM readTVarIO tst
        let !st = sumStat stats
        let !rtts = BV.fromList $ (concatMap (map (fromRational.toRational) . pktStatsRoundtripTimes) stats) :: BV.Vector Double
        let !mean = S.mean rtts * 1000 :: Double
        let !sent' = pktStatsSentTotal st
        let !recv' = pktStatsRecvTotal st
        let !sps  = floor (toRational (sent' - psent) / dt) :: Int
        let !rps  = floor (toRational (recv' - precv) / dt) :: Int
        let !lost' = pktStatsLostTotal st
        let !err  = pktStatsConnLost st
        put (sent', recv', now)
        liftIO $ atomically $ writeTChan
                    chan LogEntry { logTimestamp     = now `diffUTCTime` initTime
                                    , logSendPerSecond = sps
                                    , logRecvPerSecond = rps
                                    , logSent          = sent'
                                    , logRecv          = recv'
                                    , logMeanRtt       = mean
                                    , logErrors        = err
                                    , logLost          = lost'
                                    }
        liftIO $ threadDelay samplingPeriod
  return ()

writeLog :: Int -> String -> TChan LogEntry -> IO ()
writeLog samplingPeriod logFileName chan =
  withFile logFileName WriteMode $ \h ->
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
      threadDelay $ samplingPeriod `div` 2

displayStats :: Int -> TChan LogEntry -> IO ()
displayStats sampligPeriod chan = do
  hSetBuffering stdout NoBuffering
  forever $ do
    log' <- atomically $ let k = do x <- readTChan chan
                                    l <- isEmptyTChan chan
                                    if l then return x
                                         else k
                        in k
    let !ts   = (fromRational.toRational.logTimestamp) log' :: Double
    let !sent = logSent log'
    let !recv = logRecv log'
    let !sps  = logSendPerSecond log'
    let !rps  = logRecvPerSecond log'
    let !lost = logLost log'
    let !err  = logErrors log'
    let !mean = logMeanRtt log'
    putStr $ printf "t: %4.3fs sent: %6d (%5d p/sec) recv: %6d (%5d p/sec) mean rtt: %4.2fms lost: %3d err: %3d  \r" ts sent sps recv rps mean lost err
    threadDelay $ sampligPeriod

#if !MIN_VERSION_base(4,6,0)
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'
#endif
