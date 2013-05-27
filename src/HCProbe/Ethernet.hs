module HCProbe.Ethernet (EthFrame(..), TestEthFrame(..)) where

import Data.Word
import Network.Openflow.Types
import Network.Openflow.Ethernet
import Network.Openflow.StrictPut

data EthFrame = EthFrame { destMAC    :: !MACAddr
                         , sourceMAC  :: !MACAddr
                         , payLoadEth :: Put
                         }

instance EthernetFrame EthFrame where
  dstMacAddress    = destMAC
  srcMacAddress    = sourceMAC
  vlanID         _ = Nothing
  typeCode       _ = 0x0800
  putPayload       = payLoadEth

data TestEthFrame = TestEthFrame{ teDestMAC   :: !MACAddr
                                , teSourceMAC :: !MACAddr
                                , teVlanID    :: Maybe VLANID
                                , teTypeCode  :: !Word16
                                , tePayLoad   :: Put
                                }

instance EthernetFrame TestEthFrame where
  dstMacAddress = teDestMAC
  srcMacAddress = teSourceMAC
  vlanID        = teVlanID
  typeCode      = teTypeCode
  putPayload    = tePayLoad
