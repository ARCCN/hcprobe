module HCProbe.FakeSwitch ( PortGen(..), FakeSwitch(..)
                          , makePort
                          , makeSwitch
                          , defaultPortGen
                          , defaultSwGen
                          , fmtMac, fmtPort, fmtSwitch
                          ) where

import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as S
import Data.Word
import Data.Bits
import System.Random
import Data.List
import Text.Printf
import Control.Monad
import Control.Monad.State

data PortGen = PortGen { pnum   :: Int
                       , pname  :: Int -> BS.ByteString
                       , rndGen :: StdGen
                       }

defaultPortGen :: StdGen -> PortGen
defaultPortGen g = PortGen 1 (BS8.pack.printf "eth%d" . pred) g

makePort :: PortGen
         -> [OfpPortConfigFlags] 
         -> [OfpPortStateFlags]
         -> [OfpPortFeatureFlags]
         -> (OfpPhyPort, PortGen)

makePort gen cfg st ft = (port, gen') 
  where pn  = pnum gen
        pnm = (pname gen) pn
        gen' = gen { pnum = pn + 1, rndGen = (snd.head.reverse) mac' }
        mac' = take 3 $ unfoldr ( \g -> Just (rand g, snd (rand g)) ) (rndGen gen)
        macbytes =  [0x00, 0x16, 0x3e] ++ map fst mac' :: [Word8]
        fmac acc b = (acc `shiftL` 8) .|. (fromIntegral b :: Word64)
        rand :: StdGen -> (Word8, StdGen)
        rand = random
        port = OfpPhyPort { ofp_port_no         = fromIntegral pn
                          , ofp_port_hw_addr    = foldl fmac 0 macbytes
                          , ofp_port_name       = pnm
                          , ofp_port_config     = S.fromList cfg
                          , ofp_port_state      = S.fromList st
                          , ofp_port_current    = S.fromList ft
                          , ofp_port_advertised = S.fromList ft
                          , ofp_port_supported  = S.fromList ft
                          , ofp_port_peer       = S.fromList ft
                          }

data SwitchGen = SwitchGen {  dpid    :: Int
                           ,  ipAddr  :: IPv4Addr
                           ,  swRnd   :: StdGen
                           }
defaultSwGen :: IPv4Addr -> StdGen -> SwitchGen
defaultSwGen ip g = SwitchGen 1 ip g

data FakeSwitch = FakeSwitch { switchFeatures :: OfpSwitchFeatures, switchIP :: IPv4Addr }

makeSwitch :: SwitchGen
           -> Int
           -> [OfpCapabilities]
           -> [OfpActionType]
           -> [OfpPortConfigFlags]
           -> [OfpPortStateFlags]
           -> [OfpPortFeatureFlags]
           -> (FakeSwitch, SwitchGen)

makeSwitch gen ports cap act cfg st ff = (FakeSwitch features (ipAddr gen), gen')
  where features = OfpSwitchFeatures { ofp_datapath_id  = fromIntegral (dpid gen)
                                     , ofp_n_buffers    = fromIntegral $ 8*ports
                                     , ofp_n_tables     = 1
                                     , ofp_capabilities = S.fromList cap
                                     , ofp_actions      = S.fromList act
                                     , ofp_ports        = pps
                                     }
        gen' = gen { dpid = (dpid gen) + 1, swRnd = (rndGen pg') }
        (pps, pg') = flip runState pg $ replicateM ports genPort
        pg = defaultPortGen (swRnd gen)

        genPort = do
          g <- get
          let (p,g') = makePort g cfg st ff
          put g'
          return p

fmtMac :: MACAddr -> String
fmtMac mac = intercalate ":" $ map (printf "%02X") bytes
  where bytes = drop 2 $ unpack64 mac

fmtPort :: OfpPhyPort -> String
fmtPort p = printf "%02d %-6s HWAddr:%18s, ST: %s, FT: %s" pno pname mac st ft
  where pno = ofp_port_no p
        pname = BS8.unpack (ofp_port_name p)
        mac = fmtMac (ofp_port_hw_addr p)
        st  = show $ S.toList (ofp_port_state p)
        ft  = show $ S.toList (ofp_port_current p)

fmtSwitch :: OfpSwitchFeatures -> String
fmtSwitch f = printf "DPID: %s, %s\n" dp cap ++ intercalate "\n" ports
  where dp  = fmtMac (ofp_datapath_id f)
        cap = show (S.toList (ofp_capabilities f))
        ports = map fmtPort (ofp_ports f) 

