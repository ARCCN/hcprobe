module Network.Openflow.Ethernet () where

import Network.Openflow.Ethernet.Types
import Data.Word

class ARPReply a where
  hardwareType          a :: Word16
  ipProtocolType        a :: word16
  hwAddressLen          a :: Word8
  ipAddressLen          a :: Word8
  opcode                a :: Word16
  senderHWAddress       a :: MACAddr
  senderIPAddress       a :: IPv4Addr
  targetHWAddress       a :: MACAddr
  targetIPAddress       a :: IPv4Addr
  
