module Network.Openflow.Ethernet.Types where

import Data.Word
import Data.Maybe
import Data.ByteString
import Data.Binary.Put

type MACAddr  = Word64
type VLANID   = Word16
type IPv4Addr = Word32

class EthernetFrame a where
  dstMacAddress  :: a -> MACAddr
  srcMacAddress  :: a -> MACAddr
  vlanID         :: a -> Maybe VLANID
  typeCode       :: a -> Word16
  putPayload     :: a -> PutM ()

