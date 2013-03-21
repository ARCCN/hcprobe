module HCProbe.Ethernet (EthFrame(..)) where

import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import qualified Data.ByteString as BS
import Network.Openflow.StrictPut

data EthFrame = EthFrame { destMAC    :: !MACAddr
                         , sourcMAC   :: !MACAddr
                         , payLoadEth :: !BS.ByteString
                         }



instance EthernetFrame EthFrame where
  dstMacAddress    = destMAC
  srcMacAddress    = sourcMAC
  vlanID         _ = Nothing
  typeCode       _ = 0x0800
  putPayload       = putByteString.payLoadEth

