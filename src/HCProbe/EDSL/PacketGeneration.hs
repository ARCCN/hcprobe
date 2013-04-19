module HCProbe.EDSL.PacketGeneration
  ( -- * openflow functions
    putOFMessage
    -- * header
  , putOFHeader
  , putHdrVersion
  , putHdrType
  , putHdrLength
  , putHdrXid
    -- * payload
  , putRaw
  , putPacketIn
  , putPacketInBufferId
  , putPacketInPort
  , putPacketInReason
  , putPacketInData
  ) where

import Control.Monad.Writer
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default
import Data.Monoid
import Data.Word

import Network.Openflow.Types
import Network.Openflow.Messages
import Network.Openflow.StrictPut


putOFMessage :: Writer (Endo OfpMessage) a -> Put 
putOFMessage w = putMessage (appEndo (execWriter w) def)

-- | Create header
putOFHeader :: Writer (Endo OfpHeader) a -> Writer (Endo OfpMessage) ()
putOFHeader w = tell . Endo $ \m -> m{ofp_header = appEndo (execWriter w) (ofp_header m)}

-- | Add version
putHdrVersion :: Word8 -> Writer (Endo OfpHeader) ()
putHdrVersion v = tell . Endo $ \h -> h{ofp_hdr_version = v}

-- | Add type
putHdrType :: OfpType -> Writer (Endo OfpHeader) ()
putHdrType t = tell . Endo $ \h -> h{ofp_hdr_type = t}

-- | FIXEME maybe we need to add default length
putHdrLength :: Word16 -> Writer (Endo OfpHeader) ()
putHdrLength l = tell . Endo $ \h -> h{ofp_hdr_length = l}

-- | Add xid
putHdrXid :: Word32 -> Writer (Endo OfpHeader) ()
putHdrXid x = tell . Endo $ \h -> h{ofp_hdr_xid = x}

putRaw :: ByteString -> Writer (Endo OfpMessage) ()
putRaw bs = tell . Endo $ \m -> m{ofp_data = OfpMessageRaw bs}

putPacketIn w = tell . Endo $ \m -> m{ofp_data = OfpPacketInReply (appEndo (execWriter w) def)}

putPacketInReason :: OfpPacketInReason -> Writer (Endo OfpPacketIn) ()
putPacketInReason r = tell . Endo $ \m -> m{ofp_pkt_in_reason = r}

putPacketInPort :: Word16 -> Writer (Endo OfpPacketIn) ()
putPacketInPort p = tell . Endo $ \m -> m{ofp_pkt_in_in_port = p}

putPacketInBufferId :: Word32 -> Writer (Endo OfpPacketIn) ()
putPacketInBufferId i = tell . Endo $ \m -> m{ofp_pkt_in_buffer_id = i}

putPacketInData :: Put -> Writer (Endo OfpPacketIn) ()
putPacketInData p = tell . Endo $ \m -> m{ofp_pkt_in_data = p}

