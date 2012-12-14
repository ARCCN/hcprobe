module Network.Openflow.Ethernet.Generator where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Data.Binary.Put

putEthernetFrame :: EthernetFrame a => a -> PutM ()
putEthernetFrame x = do
  putMAC (srcMacAdddress x)
  putMAC (dstMacAddress  x)
  case (vlanID x) of
    Nothing    -> putWord16be (typeCode x)
    Just (vid) -> putWord16be 0x8100 >> putWord16be vid >> putWord16be (typeCode x)
  putPayload x
  undefined -- putCRC ??

