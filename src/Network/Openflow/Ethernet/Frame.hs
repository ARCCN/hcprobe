module Network.Openflow.Ethernet.Frame
  ( EthernetFrame(..)
  ) where

import Data.Word
import Network.Openflow.StrictPut
import Network.Openflow.Ethernet.Types

-- TODO: use uniplate/biplate to make putPayload polymorphic

class EthernetFrame a where
  dstMacAddress  :: a -> MACAddr
  srcMacAddress  :: a -> MACAddr
  vlanID         :: a -> Maybe VLANID
  typeCode       :: a -> Word16
  putPayload     :: a -> PutM ()

