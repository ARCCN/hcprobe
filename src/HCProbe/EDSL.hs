{-# LANGUAGE OverloadedStrings #-}
module HCProbe.EDSL
  ( -- * Entities creation
--    config
    -- ** EDSL for switch creation
  {-,-} 
    switchOn
  , switch
    -- ** EDSL for features
  , addMACs
  , features 
  , PortNameGen(..)
  , addPort
    -- * 
  , withSwitch
  , hangOn
  , waitForType
  -- * reexports
  , HCProbe.FakeSwitch.runSwitch
  , module Data.Default
  , module Network.Openflow.Types
  , module HCProbe.FakeSwitch.Processing
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import qualified Control.Concurrent (yield)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Writer
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import Data.Bits
import Data.Conduit
import Data.Conduit.BinaryParse
import Data.Conduit.Mutable
import Data.Conduit.Network
import Data.Conduit.TQueue
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Util as CU
import Data.Default
import Data.List
import Data.Monoid
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as M
import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.IPv4
import Network.Openflow.Messages
import Network.Openflow.StrictPut
import HCProbe.FakeSwitch
import HCProbe.FakeSwitch.Processing
import Text.Printf
import qualified System.Random.Mersenne as MR

-- | Create switch
switch :: IPv4Addr                                         -- ^ Switch address
       -> WriterT (Endo EFakeSwitch) IO a                   -- ^ Switch builder
       -> IO EFakeSwitch
switch ip = switchOn 
  EFakeSwitch { eSwitchIP = ip
              , eMacSpace = M.empty
              , eSwitchFeatures = def
              }

-- | Modify switch using builder
switchOn :: EFakeSwitch                                     -- ^ Existing switch
         -> WriterT (Endo EFakeSwitch) IO a                 -- ^ Switch Builder
         -> IO EFakeSwitch
switchOn s b = do en <- execWriterT b 
                  return (appEndo en s)

features :: WriterT (Endo OfpSwitchFeatures) IO a -> WriterT (Endo EFakeSwitch) IO ()
features w = do
    ep <- lift $ execWriterT w
    tell $ Endo (\p -> p{eSwitchFeatures = appEndo ep (eSwitchFeatures p)})

newtype PortNameGen = PortNameGen (Int -> ByteString)

instance Default PortNameGen where
    def = PortNameGen (\i -> BS8.pack $ printf "eth%d" i)

addPort :: [OfpPortConfigFlags]                 -- ^ config flags
        -> [OfpPortStateFlags]                  -- ^ state flags
        -> [OfpPortFeatureFlags]                -- ^ feature flags
        -> PortNameGen
        -> WriterT (Endo OfpSwitchFeatures) IO ()
addPort confFlags stateFlags featureFlags (PortNameGen genname) = do
    bytes <- lift $ replicateM 3 (MR.randomIO :: IO Word8)
    tell $ Endo $ \f ->
        --TODO: store mac in db?
        let pps  = ofp_ports f                            -- load existsing ports
            n    = length pps
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
        in f{ofp_ports = pps++[port]}
  where 
    fmac acc b = (acc `shiftL` 8) .|. (fromIntegral b::Word64)

addMACs :: [MACAddr] -> WriterT (Endo EFakeSwitch) IO ()
addMACs ms = tell $ Endo (\p ->
        let nmacs  = length ms
            nport  = length $! ofp_ports (eSwitchFeatures p)
            nmacpp = nmacs `div` nport
            macll  = take nport $ unfoldr (Just.(splitAt nmacpp)) ms
            ms'    = M.fromList $ zip [1..nport] (map V.fromList macll)
        in p{eMacSpace = M.unionWith (V.++) ms' (eMacSpace p)})

instance Default OfpSwitchFeatures where
  def = OfpSwitchFeatures { ofp_datapath_id  = 0
                          , ofp_n_buffers    = maxBuffers 
                          , ofp_n_tables     = 1
                          , ofp_capabilities = listToFlags ofCapabilities []
                          , ofp_actions      = listToFlags ofActionType defActions
                          , ofp_ports        = []
                          }

-- | User environment
data UserEnv = UserEnv (TVar (Sink (OfpType,OfpMessage) IO ())) (TQueue OfpMessage)

type FakeSwitchM a = ReaderT UserEnv IO a

hangOn :: ReaderT UserEnv IO a
hangOn = lift (forever Control.Concurrent.yield)

waitForType :: OfpType -> FakeSwitchM (OfpMessage)
waitForType t = do
    box <- lift $ newEmptyMVar 
    UserEnv s _ <- ask
    let ns = CL.mapM (\x -> print (fst x) >> return x) 
                =$= CL.filter ((t ==) . fst) 
                =$= CL.head >>= lift . putMVar box
    lift $ do
      os <- readTVarIO s
      atomically $ writeTVar s ns
      let go = do mx <- takeMVar box
                  case mx of
                    Nothing -> go
                    Just (a,b) -> do atomically $ writeTVar s os
                                     return b

      go

-- | Run configured switch with program inside
withSwitch :: EFakeSwitch -> ByteString -> Int -> FakeSwitchM () -> IO ()
withSwitch sw host port u = runTCPClient (clientSettings port host) $ \ad -> do
  sendQ <- atomically $ newTQueue
  swCfg <- newTVarIO defaultSwitchConfig
  runResourceT $ do
    userS <- liftIO $ newTVarIO (CL.sinkNull) 
    let extract  = runPutToByteString 32768 . putMessage
        listener =  appSource ad 
            $= conduitBinary 
            =$= CL.map (\m@(OfpMessage h _) -> ((ofp_hdr_type h),m))
            -- =$= printMessage
            $$ CU.zipSinks
                    (CL.mapM (uncurry (defProcessMessage sw swCfg)) =$= CL.catMaybes =$ sinkTQueue sendQ)
                    (mutableSink userS)
        sender   = sourceTQueue sendQ $= CL.map extract $$ appSink ad
        user     = runReaderT u (UserEnv userS sendQ)
    waitThreads <- liftIO $ mapM async [void listener, sender, user]
    mapM_ (flip allocate cancel) (map return waitThreads)
    liftIO $ do
      v <- waitAnyCatchCancel waitThreads
      print $ map fst $ filter (\(i,a) -> fst v == a) $ zip [1..] waitThreads
      case snd v of
        Left e -> putStrLn (show e)
        Right _ -> return ()

randomTCP :: IO (ByteString)
randomTCP = undefined
