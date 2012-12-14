module Network.Openflow.Ethernet.Generator where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Data.Binary.Put

putEthernetFrame :: EthernetFrame a => a -> PutM ()
putEthernetFrame x = do
  putMAC (srcMacAdddress x)
  putMAC (dstMacAddress  x)
  case (vlanID x) of
    Nothing    -> undefined
    Just (vid) -> undefined
  undefined -- putCRC ??

