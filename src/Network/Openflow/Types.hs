module Network.Openflow.Types ( OfpHeader(..), OfpType(..), OfpMessage(..), OfpMessageData(..)
                              , OfpCapabilities(..)
                              ) where
 
import Data.Word
import qualified Data.ByteString as BS
import Data.Bits

type MACAddr = Word64

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



data OfpSwitchFeatures = OfpSwitchFeatures { ofp_datapath_id  :: Word64
                                           , ofp_n_buffers    :: Word32
                                           , ofp_n_tables     :: Word32
                                           , ofp_capabilities :: [OfpCapabilities]
                                           , ofp_actions      :: [OfpActionType]
                                           , ofp_ports        :: [OfpPhyPort]
                                           }

data OfpCapabilities =   OFPC_FLOW_STATS             --  Flow statistics
                       | OFPC_TABLE_STATS            --  Table statistics
                       | OFPC_PORT_STATS             --  Port statistics
                       | OFPC_STP                    --  802.1d spanning tree
                       | OFPC_RESERVED               --  Reserved, must be zero
                       | OFPC_IP_REASM               --  Can reassemble IP fragments
                       | OFPC_QUEUE_STATS            --  Queue statistics
                       | OFPC_ARP_MATCH_IP           --  Match IP addresses in ARP pkts
                       deriving (Eq, Ord, Show)

instance Enum OfpCapabilities where
  toEnum 1   = OFPC_FLOW_STATS
  toEnum 2   = OFPC_TABLE_STATS
  toEnum 4   = OFPC_PORT_STATS
  toEnum 8   = OFPC_STP
  toEnum 16  = OFPC_RESERVED
  toEnum 32  = OFPC_IP_REASM
  toEnum 64  = OFPC_QUEUE_STATS
  toEnum 128 = OFPC_ARP_MATCH_IP
  fromEnum OFPC_FLOW_STATS   = 1
  fromEnum OFPC_TABLE_STATS  = 2
  fromEnum OFPC_PORT_STATS   = 4
  fromEnum OFPC_STP          = 8
  fromEnum OFPC_RESERVED     = 16
  fromEnum OFPC_IP_REASM     = 32
  fromEnum OFPC_QUEUE_STATS  = 64
  fromEnum OFPC_ARP_MATCH_IP = 128

data OfpActionType = OFPAT_OUTPUT          -- Output to switch port
                     | OFPAT_SET_VLAN_VID  -- Set the 802.1q VLAN id
                     | OFPAT_SET_VLAN_PCP  -- Set the 802.1q priority
                     | OFPAT_STRIP_VLAN    -- Strip the 802.1q header
                     | OFPAT_SET_DL_SRC    -- Ethernet source address
                     | OFPAT_SET_DL_DST    -- Ethernet destination address
                     | OFPAT_SET_NW_SRC    -- IP source address
                     | OFPAT_SET_NW_DST    -- IP destination address
                     | OFPAT_SET_NW_TOS    -- IP ToS (DSCP field 6 bits)
                     | OFPAT_SET_TP_SRC    -- TCP/UDP source port
                     | OFPAT_SET_TP_DST    -- TCP/UDP destination port
                     | OFPAT_ENQUEUE       -- Output to queue
                     | OFPAT_VENDOR
                    deriving(Eq, Ord, Show)

instance Enum OfpActionType where
  toEnum 0 = OFPAT_OUTPUT 
  toEnum 1 = OFPAT_SET_VLAN_VID 
  toEnum 2 = OFPAT_SET_VLAN_PCP 
  toEnum 3 = OFPAT_STRIP_VLAN 
  toEnum 4 = OFPAT_SET_DL_SRC 
  toEnum 5 = OFPAT_SET_DL_DST 
  toEnum 6 = OFPAT_SET_NW_SRC 
  toEnum 7 = OFPAT_SET_NW_DST 
  toEnum 8 = OFPAT_SET_NW_TOS 
  toEnum 9 = OFPAT_SET_TP_SRC 
  toEnum 10 = OFPAT_SET_TP_DST 
  toEnum 11 = OFPAT_ENQUEUE 
  toEnum 0xFFFF = OFPAT_VENDOR
  fromEnum OFPAT_OUTPUT        = 0
  fromEnum OFPAT_SET_VLAN_VID  = 1
  fromEnum OFPAT_SET_VLAN_PCP  = 2
  fromEnum OFPAT_STRIP_VLAN    = 3
  fromEnum OFPAT_SET_DL_SRC    = 4
  fromEnum OFPAT_SET_DL_DST    = 5
  fromEnum OFPAT_SET_NW_SRC    = 6
  fromEnum OFPAT_SET_NW_DST    = 7
  fromEnum OFPAT_SET_NW_TOS    = 8
  fromEnum OFPAT_SET_TP_SRC    = 9
  fromEnum OFPAT_SET_TP_DST    = 10
  fromEnum OFPAT_ENQUEUE       = 11
  fromEnum OFPAT_VENDOR        = 0xFFFF

data OfpPhyPort = OfpPhyPort { ofp_port_no       :: Word16
                             , ofp_port_hw_addr  :: MACAddr
                             , ofp_port_name     :: BS.ByteString
                             , ofp_port_config   :: [OfpPortConfig]
                             , ofp_port_state    :: [OfpPortState]
                             }

