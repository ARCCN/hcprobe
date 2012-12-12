module Network.Openflow.Types ( OfpHeader(..), OfpType(..), OfpMessage(..), OfpMessageData(..)
                              , putOfpHeader
                              ) where
 
import Data.Word
import Data.Binary.Put
import qualified Data.ByteString as BS

data OfpHeader = OfpHeader { ofp_hdr_version :: Word8
                           , ofp_hdr_type    :: OfpType 
                           , ofp_hdr_length  :: Word16
                           , ofp_hdr_xid     :: Word32
                           }

data OfpMessage = OfpMessage { ofp_header  :: OfpHeader
                             , ofp_data    :: OfpMessageData
                             }

data OfpMessageData = OfpMessageRaw BS.ByteString

data OfpType  = 
    -- Immutable messages
      OFPT_HELLO               -- Symmetric message
    | OFPT_ERROR               -- Symmetric message
    | OFPT_ECHO_REQUEST        -- Symmetric message
    | OFPT_ECHO_REPLY          -- Symmetric message
    | OFPT_EXPERIMENTER        -- Symmetric message

    -- Switch configuration messages
    | OFPT_FEATURES_REQUEST    -- Controller/switch message
    | OFPT_FEATURES_REPLY      -- Controller/switch message
    | OFPT_GET_CONFIG_REQUEST  -- Controller/switch message
    | OFPT_GET_CONFIG_REPLY    -- Controller/switch message
    | OFPT_SET_CONFIG          -- Controller/switch message

    -- Asynchronous messages
    | OFPT_PACKET_IN           -- Async message
    | OFPT_FLOW_REMOVED        -- Async message
    | OFPT_PORT_STATUS         -- Async message

    -- Controller command messages
    | OFPT_PACKET_OUT          -- Controller/switch message
    | OFPT_FLOW_MOD            -- Controller/switch message
    | OFPT_GROUP_MOD           -- Controller/switch message
    | OFPT_PORT_MOD            -- Controller/switch message
    | OFPT_TABLE_MOD           -- Controller/switch message

    -- Statistics messages
    | OFPT_STATS_REQUEST       -- Controller/switch message
    | OFPT_STATS_REPLY         -- Controller/switch message

    -- Barrier messages
    | OFPT_BARRIER_REQUEST     -- Controller/switch message
    | OFPT_BARRIER_REPLY       -- Controller/switch message

    -- Queue Configuration messages
    | OFPT_QUEUE_GET_CONFIG_REQUEST  -- Controller/switch message
    | OFPT_QUEUE_GET_CONFIG_REPLY    -- Controller/switch message

    deriving (Ord, Eq, Enum, Show)


putOfpHeader :: OfpHeader -> PutM ()
putOfpHeader h = putWord8 version >> putWord8 tp >> putWord16be len >> putWord32be xid
  where version = ofp_hdr_version h
        tp      = (fromIntegral.fromEnum.ofp_hdr_type) h
        len     = ofp_hdr_length h
        xid     = ofp_hdr_xid h

