module HCProbe.TCP () where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.IPv4
import qualified Data.ByteString as BS
import Data.Binary.Put
import qualified Data.Serialize as S
import Data.Word

data TestPacketTCP = TestPacketTCP { dstMAC  :: MACAddr
                                   , srcMAC  :: MACAddr
                                   , srcIP   :: IPv4Addr
                                   , dstPort :: Word16
                                   , srcPort :: Word16
                                   , payLoad :: BS.ByteString
                                   }


--instance EthernetFrame TestPacketTCP where
--  dstMacAddress = dstMAC 
--  srcMacAddress = srcMAC
--  vlanID        = const Nothing
--  typeCode      = const 0x800
--  putPayload    = putTestPacketTCP

--putTestPacketTCP :: TestPacketTCP -> PutM ()
--putTestPacketTCP = undefined
--  where ipHeader = dummyIPv4Header { totalLength = undefined
--                                   }

