{-# LANGUAGE OverloadedStrings, FlexibleContexts, CPP #-}
module HCProbe.EDSL
  ( -- * Entities creation
--    config
    -- ** EDSL for switch creation
  {-,-}
    switchOn
  , switch
  , config
    -- ** EDSL for features
  , features
  , addPort
  , PortNameGen(..)
    -- ** MAC adddress management
  , addMACs
  , clearMACs
  , ensureUnique
    -- * Runtime operations
    -- * Types
  , FakeSwitchM
  , withSwitch
  , setUserHandler
  , HCProbe.FakeSwitch.runSwitch
    -- ** Block operations
  , hangOn
  , waitForType
  , waitForBID
  , delay
    -- ** Packet sending
  , send
  , sendOFPPacketIn
  , arpGreeting
    -- ** Helpers
    -- *** Mac lists
  , portLength
  , currentSwitchConfig
  , genMassiveMACs
  , genPerPortMACs
    -- *** Counters
  , nextBID
  , nextXID
    -- * Sending
  -- , portMACs
  , genLocalMAC
  -- * reexports
  , MACGen
  , EFakeSwitch(..)
  , module Data.Default
  , module Network.Openflow.Types
  , module HCProbe.FakeSwitch.Processing
  , module HCProbe.EDSL.PacketGeneration
  ) where

import Control.Arrow
import Control.Applicative
import Control.Concurrent.MVar
import qualified Control.Concurrent ( yield )
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent ( threadDelay )
import Control.Exception ( bracket )
import Control.Monad.Writer
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import Control.Monad.State.Lazy
import Data.Bits
import Data.Conduit
import Data.Conduit.Serialization.Binary
import Data.Conduit.Mutable
import Data.Conduit.Network
import Data.Conduit.TQueue
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util as CU
import Data.Default
import Data.List
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Network.Openflow.Types
import Network.Openflow.Ethernet
import Network.Openflow.Messages
import Network.Openflow.StrictPut
import HCProbe.FakeSwitch
import HCProbe.FakeSwitch.Processing
import HCProbe.EDSL.PacketGeneration
import Text.Printf
import qualified System.Random.Mersenne as MR

data SwitchState = SwitchState (S.Set MACAddr)

config :: StateT SwitchState IO a
       -> IO a
config = flip evalStateT (SwitchState S.empty)

-- | create switch from default using builder
switch :: (Monad m)
       => WriterT (Endo EFakeSwitch) m a
       -> m EFakeSwitch
switch = switchOn def

-- | Modify switch using builder
switchOn :: (Monad m)
         => EFakeSwitch                                     -- ^ Existing switch
         -> WriterT (Endo EFakeSwitch) m a                 -- ^ Switch Builder
         -> m EFakeSwitch
switchOn s b = do en <- execWriterT b
                  return (appEndo en s)

-- | Create switch features
features :: WriterT (Endo OfpSwitchFeatures) (StateT SwitchState IO) a
         -> WriterT (Endo EFakeSwitch) (StateT SwitchState IO) ()
features w = do
    ep <- lift $ execWriterT w
    tell $ Endo (\p -> p{eSwitchFeatures = appEndo ep (eSwitchFeatures p)})

-- | Port name generator
newtype PortNameGen = PortNameGen (Int -> ByteString)

instance Default PortNameGen where
    def = PortNameGen (\i -> BS8.pack $ printf "eth%d" i)

-- | Create new port on switch
addPort :: (MonadIO m)
        => [OfpPortConfigFlags]                 -- ^ config flags
        -> [OfpPortStateFlags]                  -- ^ state flags
        -> [OfpPortFeatureFlags]                -- ^ feature flags
        -> PortNameGen
        -> WriterT (Endo OfpSwitchFeatures) m ()
addPort confFlags stateFlags featureFlags (PortNameGen genname) = do
    bytes <- liftIO $ replicateM 3 (MR.randomIO :: IO Word8)
    tell $ Endo $ \f ->
        --TODO: store mac in db?
        let pps  = ofp_switch_features_ports f                            -- load existsing ports
            n    = (length pps) + 1
            macbytes = [0x00, 0x16,0x3e] ++ bytes
            port = OfpPhyPort
                     { ofp_port_no = fromIntegral n
                     , ofp_port_hw_addr    = foldl fmac 0 macbytes
                     , ofp_port_name       = genname n
                     , ofp_port_config     = listToFlags ofConfigFlags confFlags
                     , ofp_port_state      = listToFlags ofStateFlags stateFlags
                     , ofp_port_current    = listToFlags ofFeatureFlags featureFlags
                     , ofp_port_advertised = listToFlags ofFeatureFlags featureFlags
                     , ofp_port_supported  = listToFlags ofFeatureFlags featureFlags
                     , ofp_port_peer       = listToFlags ofFeatureFlags featureFlags
                     }
        in f{ofp_switch_features_ports = pps++[port]}
  where
    fmac acc b = (acc `shiftL` 8) .|. (fromIntegral b::Word64)

-- | Create unique MAC addr if mac is not unique in store then new mac will
-- be created. Returns mac that is unique
ensureUnique :: (MonadState SwitchState m)
             => MACAddr
             -> WriterT (Endo EFakeSwitch) m MACAddr
ensureUnique a = do
    (SwitchState st) <- get
    return $ until (\m->not $ S.member m st) (\m->m+1) a

-- | Add MACs to switch, MACs will be divided between ports
addMACs :: (MonadState SwitchState m)
        => [MACAddr]
        -> WriterT (Endo EFakeSwitch) m ()
addMACs ms' = do
    ms <- sequence $ map ensureUnique ms'
    tell $ Endo (\p ->
        let nmacs  = length ms
            nport  = length $! ofp_switch_features_ports (eSwitchFeatures p)
            nmacpp = nmacs `div` nport
            macll  = take nport $ unfoldr (Just.(splitAt nmacpp)) ms
            ms''   = IM.fromList $ zip [1..nport] (map V.fromList macll)
        in p{eMacSpace = IM.unionWith (V.++) ms'' (eMacSpace p)})


-- | remove all MAC addresses connected to switch
clearMACs :: (Monad m) => WriterT (Endo EFakeSwitch) m ()
clearMACs = tell $ Endo (\p -> p{eMacSpace = IM.empty})

defaultFeatures :: OfpSwitchFeatures
defaultFeatures = OfpSwitchFeatures { ofp_switch_features_datapath_id  = 0
                                   , ofp_switch_features_n_buffers    = maxBuffers
                                   , ofp_switch_features_n_tables     = 1
                                   , ofp_switch_features_capabilities = listToFlags ofCapabilities []
                                   , ofp_switch_features_actions      = listToFlags ofActionType defActions
                                   , ofp_switch_features_ports        = []
                                   }

-- | Mac generation state
data MACGen = None -- at begin
            | Massive Int Int -- running througth all mac in switch
            | PerPort (IM.IntMap Int) --  - || - in port

-- | User environment
data UserEnv = UserEnv
        { switchConfig  :: EFakeSwitch
        , runtimeConfig :: TVar OfpSwitchConfig
        , currentBID    :: IORef Word32
        , currentXID    :: IORef Word32
        , userSink      :: TVar (Sink (OfpType,OfpMessage) IO ())
        , userHandler   :: TVar ((OfpType, OfpMessage) -> IO (OfpType, OfpMessage))
        , queue         :: TQueue OfpMessage
        , macGen        :: MACGen
        }

type FakeSwitchM a = ReaderT UserEnv IO a

-- | Wait forever
hangOn :: ReaderT UserEnv IO a
hangOn = lift (forever Control.Concurrent.yield)

-- | Wait for concrete message type
waitForType :: OfpType -> FakeSwitchM (OfpMessage)
waitForType t = fmap snd . withUserSink $ \box ->
  CL.mapM (\x -> print (fst x) >> return x)
       =$= CL.filter ((t ==) . fst)
       =$= CL.head >>= lift . putMVar box

-- | Wait for message with buffer id specified.
waitForBID :: Word32 -> FakeSwitchM (OfpMessage)
waitForBID b = withUserSink $ \box ->
  let loop = do
      mx <- await
      case mx of
        Nothing -> lift $ putMVar box Nothing
        Just (_,m@(OfpMessage _ (OfpPacketOut (OfpPacketOutData b' _))))
            | b == b' -> lift (putMVar box (Just m)) >> return ()
        _ -> loop
  in loop

withUserSink :: (MVar (Maybe b) -> Sink (OfpType, OfpMessage) IO ()) -> FakeSwitchM b
withUserSink u = do
  s <- asks userSink
  lift $ do
    box <- newEmptyMVar
    bracket (atomically $ readTVar s >>= \x -> writeTVar s (u box) >> return x)
            (\o -> atomically $ writeTVar s o)
            (\_ -> let go = takeMVar box >>= maybe go return in go)

setUserHandler :: ( (OfpType, OfpMessage) -> IO (OfpType, OfpMessage) )
                -> FakeSwitchM ()
setUserHandler h = do
  tHandler <- asks userHandler
  liftIO $ atomically $ writeTVar tHandler h
  -- realy need next?
  withReaderT (\s->s{userHandler = tHandler}) $ return ()

-- | get next buffer id
nextBID :: FakeSwitchM Word32
nextBID = do
    (cfg, bbox) <- asks (switchConfig &&& currentBID)
    let nbuf = (ofp_switch_features_n_buffers . eSwitchFeatures) cfg
    lift $ atomicModifyIORef' bbox (\c -> (if c+1>nbuf then 1 else c+1, c))

-- | Get next transaction id
nextXID :: FakeSwitchM Word32
nextXID = do
    xid <- asks currentXID
    lift $ atomicModifyIORef' xid (\c -> (c+1, c) )

portLength :: FakeSwitchM Int
portLength = asks ( IM.size . eMacSpace . switchConfig)

-- | Get actual switch configuration (could be changed by controller)
currentSwitchConfig :: FakeSwitchM OfpSwitchConfig
currentSwitchConfig = asks runtimeConfig >>= lift . readTVarIO

-- | Send openflow message
send :: OfpMessage -> FakeSwitchM ()
send m = do
  q <- asks queue
  lift . atomically $ writeTQueue q m

-- | Send Open flow PacketIn message
sendOFPPacketIn :: Word16   -- ^ port id
                -> PutM ()
                -> FakeSwitchM Word32
sendOFPPacketIn pid pl = do
        q <- asks queue
        bid <- nextBID
        xid <- nextXID
        lift . atomically . writeTQueue q $
                  OfpMessage (header openflow_1_0 xid OFPT_PACKET_IN)
                             (OfpPacketInReply (OfpPacketIn bid pid OFPR_NO_MATCH pl))
        return bid

arpGreeting :: FakeSwitchM ()
arpGreeting = do
    (q, fk) <- asks (queue &&& switchConfig)
    xid <- nextXID
    lift . atomically . writeTQueue q $ eArpGrat fk (-1 :: Word32) xid

-- | Delay in nanoseconds
delay :: Int -> FakeSwitchM ()
delay = lift . threadDelay

-- | maximum OF message size in bytes limited because lengthes are uint16
maxMessageSize :: Int
maxMessageSize = 65356


-- | Run configured switch with program inside
withSwitch :: EFakeSwitch
           -> ByteString
           -> Int
--           -> Maybe (Conduit (OfpType, OfpMessage) IO (OfpType, OfpMessage) )
           -> FakeSwitchM ()
           -> IO ()
withSwitch sw host port u = runTCPClient (clientSettings port host) $ \ad -> do
  sendQ <- atomically $ newTQueue
  ref   <- newIORef 0
  swCfg <- newTVarIO defaultSwitchConfig
  runResourceT $ do
    userS <- liftIO $ newTVarIO (CL.sinkNull)
    userH <- liftIO $ newTVarIO $ (\m->return m)
    let extract'  = runPutToByteString maxMessageSize . putMessage
        listener =  appSource ad
            $= conduitDecode
            =$= ( CL.map (\m@(OfpMessage h _) -> ((ofp_hdr_type h),m)) :: Conduit OfpMessage IO (OfpType, OfpMessage) )
            =$= CL.mapM (\m -> do
                            action <- readTVarIO userH
                            action m
                        )
--            =$= printMessage
            $$ CU.zipSinks
                    (CL.mapM (uncurry (defProcessMessage sw swCfg))
                        =$= CL.catMaybes =$ sinkTQueue sendQ)
                    (mutableSink userS)
        sender   = sourceTQueue sendQ $= CL.map extract' $$ appSink ad
--                     $= CL.mapM (\x -> putStr "> " >> print x >> return x)
--                     =$= CL.map extract'
--                     =$= CL.mapM (\x -> putStrLn ("SENDING:" ++ show x) >> return x)
--                     $$ appSink ad
        user     = runReaderT u (UserEnv sw swCfg ref ref userS userH sendQ None)
    liftIO . atomically $ writeTQueue sendQ (headReply def OFPT_HELLO)
    waitThreads <- liftIO $ mapM async [void listener, sender, user]
    mapM_ (flip allocate cancel) (map return waitThreads)
    liftIO $ do
      v <- waitAnyCatchCancel waitThreads
      case snd v of
         Left e -> putStrLn (show e)
         Right _ -> return ()
  where takeUserHandler Nothing = CL.map (\m -> m)
        takeUserHandler (Just h) = h

genLocalMAC :: FakeSwitchM MACAddr
genLocalMAC = do
   st <- asks switchConfig
   let nm = IM.size $ eMacSpace st
   em <- liftIO $ liftM (`mod` nm) MR.randomIO -- gen position in Map of random Port
   let macs = (IM.elems $ eMacSpace st) !! em
       nv = V.length macs
   ev <- liftIO $ liftM (`mod` nv) MR.randomIO -- gen position in V of random Mac
   return (macs V.! ev)

checkMACGen :: (MACGen -> FakeSwitchM ()) -> FakeSwitchM ()
checkMACGen f = do
    st <- asks macGen
    f st

switchMACStateMass :: Monad m => MACGen -> ReaderT UserEnv m ()
switchMACStateMass st =
    case st of
        None -> emptyMACState
        (PerPort _) -> emptyMACState
        (Massive _ _) -> return ()
    where emptyMACState = withReaderT
                (\ue -> ue{macGen = Massive 0 0} )
                $ return ()

switchMACStatePort :: Monad m => MACGen -> ReaderT UserEnv m ()
switchMACStatePort st =
    case st of
        None -> emptyMACState
        (PerPort _) -> return ()
        (Massive _ _) -> emptyMACState
    where emptyMACState = withReaderT
                (\ue -> ue{macGen = PerPort IM.empty} )
                $ return ()

genMassiveMACs :: FakeSwitchM MACAddr
genMassiveMACs = do
    checkMACGen switchMACStateMass
    sw <- asks switchConfig
    (Massive mi vi) <- asks macGen
    _ <- evalState'
    return $ ((eMacSpace sw) IM.! mi) V.! vi
    where evalState' = do
            withReaderT nextStateElem $ ask
          nextStateElem ue =
            let (Massive mi vi) = macGen ue
                m = eMacSpace $ switchConfig ue
                v = m IM.! mi
                (mi', vi') = checkNextStateElem m v mi (vi+1)
            in ue{macGen = Massive mi' vi'}
          checkNextStateElem m v mi vi =
            let keys = IM.keys m
                maxk = last keys
                maxv = V.length $ v
            in if (not $ IM.member mi m)
                then if ( mi >= maxk )
                        then checkNextStateElem m v (head keys) 0
                        else checkNextStateElem m v (mi+1) 0
                else if ( vi >= maxv )
                        then checkNextStateElem m v (mi+1) 0
                        else (mi, vi)

genPerPortMACs :: Int -> FakeSwitchM MACAddr
genPerPortMACs port = do
   checkMACGen switchMACStatePort
   sw <- asks switchConfig
   (PerPort m) <- asks macGen
   let n = if IM.member port m
            then m IM.! port
            else 0
   _ <- evalState'
   return $ ((eMacSpace sw) IM.! port) V.! n
   where evalState' = do
            withReaderT nextStateElem $ ask
         nextStateElem ue =
            let (PerPort mg) = macGen ue
                m = eMacSpace $ switchConfig ue
                mac = mg IM.! port
            in if ( IM.member port m )
                then let v = m IM.! port
                         vl = V.length v
                     in if ( mac+1 >= vl )
                         then ue{macGen = PerPort $ IM.insert port 0 mg}
                         else ue{macGen = PerPort $ IM.insert port (mac+1) mg}
                else ue{macGen = PerPort $ IM.insert port 0 mg}

instance Default EFakeSwitch where
  def = EFakeSwitch defaultFeatures def

#if ! MIN_VERSION_base(4,6,0)
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
    		(\x -> let (a, b) = f x
			in (a, a `seq` b))
    b `seq` return b
#endif
