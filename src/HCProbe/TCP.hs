{-# Language BangPatterns #-}
module HCProbe.TCP (TestPacketTCP(..)) where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.IPv4
import Network.Openflow.Ethernet.TCP
import qualified Data.ByteString as BS
import Nettle.OpenFlow.StrictPut 
import Data.Word

data TestPacketTCP = TestPacketTCP { dstMAC    :: !MACAddr
                                   , srcMAC    :: !MACAddr
                                   , srcIP     :: !IPv4Addr
                                   , dstIP     :: !IPv4Addr
                                   , dstPort   :: !Word16
                                   , srcPort   :: !Word16
                                   , testIpID  :: !(Maybe Int)
                                   , testSeqNo :: !(Maybe Int)
                                   , testAckNo :: !(Maybe Int)
                                   , testWSS   :: !(Maybe Int)
                                   , testFlags :: !(Maybe [TCPFlag])
                                   , payLoad   :: !BS.ByteString
                                   }


instance EthernetFrame TestPacketTCP where
  dstMacAddress = dstMAC 
  srcMacAddress = srcMAC
  vlanID        = const Nothing
  typeCode      = const 0x800
  putPayload    = putIPv4Pkt


instance IPv4 TestPacketTCP where
  ipHeaderLen  = const 5
  ipVersion    = const 4
  ipTOS        = const 0
  ipID x       = maybe 0 fromIntegral (testIpID x)
  ipFlags      = const 0 
  ipFragOffset = const 0
  ipTTL        = const 255
  ipProto      = const 6 -- TCP
  ipSrc        = srcIP
  ipDst        = dstIP
  ipPutPayload = putTCP

instance TCP TestPacketTCP where
  tcpSrcAddr    = ipSrc
  tcpDstAddr    = ipDst
  tcpProto      = ipProto
  tcpSrcPort    = srcPort
  tcpDstPort    = dstPort
  tcpSeqNo      = (maybe 0 fromIntegral).testSeqNo
  tcpAckNo      = (maybe 0 fromIntegral).testAckNo
  tcpFlags      = (maybe [] id).testFlags
  tcpWinSize    = (maybe 0 fromIntegral).testWSS
  tcpUrgentPtr  = const 0 
  tcpPutPayload = putByteString.payLoad

