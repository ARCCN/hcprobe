module HCProbe.EDSL.Handlers
    ( OfpPredicate
    , predicateHandler
    , PktStats(..)
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
                         , pktStatsRoundtripTime :: !(Maybe NominalDiffTime)
                         } deriving Show
data StatsEntity = StatsEntity { packetQ :: TVarL (IM.IntMap UTCTime)
                               , stats   :: TVar PktStats
                               }
initPacketStats :: (MonadIO m)
                => m StatsEntity
initPacketStats = do
    im <- liftIO $ newTVarIO IM.empty
    pQ <- liftIO $ newTVarIO (0,im)
    st <- liftIO $ newTVarIO (PktStats 0 0 0 0 Nothing)
    return $ StatsEntity pQ st

setStatsHandler :: StatsEntity
                -> FakeSwitchM ()
setStatsHandler pS = setUserHandler $ statsHandler pS

statsHandler :: StatsEntity 
             -> (OfpType, OfpMessage) 
             -> IO (OfpType, OfpMessage)
statsHandler (StatsEntity q s) 
             m@(_, (OfpMessage _ (OfpPacketOut (OfpPacketOutData bid _pid)))) = do
  now <- getCurrentTime
  pq  <- readTVarIO . snd =<< readTVarIO q
  whenJustM (IM.lookup ibid pq) $ \dt -> do
    atomically $ do
      modifyTVar s (\st -> st { pktStatsSentTotal = succ (pktStatsSentTotal st)
                                , pktStatsRoundtripTime = Just( now `diffUTCTime` dt )
                                })
      (l,pq') <- readTVar q
      modifyTVar pq' $ IM.delete ibid
      writeTVar q (l-1,pq')
  return m
  where ibid = fromIntegral bid
statsHandler _ m = return m

--FIXME: remove hardcoded parameters
statsOnSend :: (MonadIO m) 
            => StatsEntity
            -> OfpMessage
            -> m ()
statsOnSend (StatsEntity q s) 
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
      when (l > 1024) $ do
        pq <- readTVar g
        let rationalTimeout = toRational 1
        let (_lost, rest) = IM.partition ((>rationalTimeout).toRational.diffUTCTime now) pq
        writeTVar g $! rest
        modifyTVar s (\st -> st { pktStatsLostTotal = succ (pktStatsLostTotal st)
                                  })
        writeTVar q (IM.size rest,g)
 
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
getStats (StatsEntity _ s) = 
    liftIO $ readTVarIO s

assembleStats :: (MonadIO m)
              => [StatsEntity]
              -> m PktStats
assembleStats lSE = liftIO $ do
  lPS <- sequence $ map getStats lSE
  let summ = foldl1 addStats lPS 
      len  = length lPS
  return $ summ{ pktStatsRoundtripTime 
        = (\a -> return $ a / (fromIntegral len) ) 
            =<< (pktStatsRoundtripTime summ)}
  where
    addStats (PktStats s r l cl rtt ) (PktStats as ar al acl artt) = 
      PktStats (s+as) (r+ar) (l+al) (cl+acl) (liftM2 (+) rtt artt)
 
