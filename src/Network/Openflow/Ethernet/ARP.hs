module Network.Openflow.Ethernet.ARP ( ARPReply(..)
                                     , putARPReply
                                     )where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc            -- TODO move to Network.Openflow.Ethernet.Put

import Data.Word
import Network.Openflow.StrictPut

-- | Class describing ARP message
class ARPReply a where
  hardwareType    :: a -> Word16
  ipProtocolType  :: a -> Word16
  hwAddressLen    :: a -> Word8
  ipAddressLen    :: a -> Word8
  opcode          :: a -> Word16
  senderHWAddress :: a -> MACAddr
  senderIPAddress :: a -> IPv4Addr
  targetHWAddress :: a -> MACAddr
  targetIPAddress :: a -> IPv4Addr

putARPReply :: ARPReply a => a -> PutM ()
putARPReply x = do
  putWord16be (hardwareType x)
  putWord16be (ipProtocolType x)
  putWord8    (hwAddressLen x)
  putWord8    (ipAddressLen x)
  putWord16be (opcode x)
  putMAC      (senderHWAddress x)
  putIP       (senderIPAddress x)
  putMAC      (targetHWAddress x)
  putIP       (targetIPAddress x)

