module HCProbe.ARP where

import Network.Openflow.Types
import Network.Openflow.Ethernet

data ARPGratuitousReply = ARPGratuitousReply { senderMAC :: !MACAddr
                                             , senderIP  :: !IPv4Addr
                                             }
                          deriving (Show)

instance ARPReply ARPGratuitousReply where
  hardwareType    _ = 0x01
  ipProtocolType  _ = 0x800
  hwAddressLen    _ = 6
  ipAddressLen    _ = 4
  opcode          _ = 0x02
  senderHWAddress = senderMAC
  senderIPAddress = senderIP
  targetHWAddress = senderMAC
  targetIPAddress = senderIP

instance EthernetFrame ARPGratuitousReply where
  dstMacAddress  _ = 0xFFFFFFFFFFFF
  srcMacAddress    = senderMAC
  vlanID         _ = Nothing
  typeCode       _ = 0x806
  putPayload       = putARPReply

