module HCProbe.Ethernet (EthFrame(..), EthFrameP(..)) where

import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import qualified Data.ByteString as BS
import Network.Openflow.StrictPut

data EthFrame = EthFrame { destMAC    :: !MACAddr
                         , sourcMAC   :: !MACAddr
                         , payLoadEth :: !Put
                         }

instance EthernetFrame EthFrame where
  dstMacAddress    = destMAC
  srcMacAddress    = sourcMAC
  vlanID         _ = Nothing
  typeCode       _ = 0x0800
  putPayload       = payLoadEth

data EthFrameP = EthFrameP (MACAddr) (MACAddr)  (Put)

instance EthernetFrame EthFrameP where
  dstMacAddress (EthFrameP a _ _)   = a
  srcMacAddress (EthFrameP _ b _)   = b
  vlanID         _ = Nothing
  typeCode       _ = 0x0800
  putPayload    (EthFrameP _ _ p)   = p
