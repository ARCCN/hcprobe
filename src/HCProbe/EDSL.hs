{-# LANGUAGE OverloadedStrings #-}
module HCProbe.EDSL
  ( -- * EDSL for switch creation
    switchOn
  , switch
  , addMACs
    -- * EDSL for features
  , features 
  , PortNameGen(..)
  , addPort
  -- * run program
  , HCProbe.FakeSwitch.runSwitch
  , withSwitch
  ) where

import Control.Monad ( forever )
import Control.Monad.Writer
import Control.Concurrent (yield)-- FIXME remove
import Data.Default
import Data.Monoid
import Data.Bits
import Data.List
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as M
import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.IPv4
import HCProbe.FakeSwitch
import Text.Printf

withSwitch = undefined

-- | Create switch
switch :: IPv4Addr                                      -- ^ Switch address
       -> Writer (Endo EFakeSwitch) a                   -- ^ Switch builder
       -> EFakeSwitch
switch ip = switchOn 
  EFakeSwitch { eSwitchIP = ip
              , eMacSpace = M.empty
              , eSwitchFeatures = def
              }

-- | Modify switch using builder
switchOn :: EFakeSwitch                                 -- ^ Existing switch
         -> Writer (Endo EFakeSwitch) a                 -- ^ Switch Builder
         -> EFakeSwitch
switchOn s b = appEndo (execWriter b) s

features :: Writer (Endo OfpSwitchFeatures) a -> Writer (Endo EFakeSwitch) ()
features w = tell $ Endo (\p -> p{eSwitchFeatures = appEndo (execWriter w) (eSwitchFeatures p)})

newtype PortNameGen = PortNameGen (Int -> ByteString)

instance Default PortNameGen where
    def = PortNameGen (\i -> BS8.pack $ printf "eth%d" i)

addPort :: [OfpPortConfigFlags]                 -- ^ config flags
        -> [OfpPortStateFlags]                  -- ^ state flags
        -> [OfpPortFeatureFlags]                -- ^ feature flags
        -> PortNameGen
        -> Writer (Endo OfpSwitchFeatures) ()
addPort confFlags stateFlags featureFlags (PortNameGen genname) = do
    tell $ Endo $ \f ->
        let pps = ofp_ports f 
            n   = length pps
            macbytes = [0x00, 0x16,0x3e] ++ [0xDE,0xAD,0xBE] -- FIXME make random    
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

addMACs :: [MACAddr] -> Writer (Endo EFakeSwitch) ()
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

