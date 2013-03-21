module HCProbe.ARP where

import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.ARP

data ARPGratuitousReply = ARPGratuitousReply { senderMAC :: !MACAddr
                                             , senderIP  :: !IPv4Addr
                                             }

instance ARPReply ARPGratuitousReply where
  hardwareType    x = 0x01
  ipProtocolType  x = 0x800
  hwAddressLen    x = 6
  ipAddressLen    x = 4
  opcode          x = 0x02
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

