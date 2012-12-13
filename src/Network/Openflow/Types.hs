module Network.Openflow.Types ( OfpHeader(..), OfpType(..), OfpMessage(..), OfpMessageData(..)
                              , OfpCapabilities(..), OfpSwitchFeatures(..), OfpPhyPort(..)
                              , OfpPortConfigFlags(..), OfpPortStateFlags(..), OfpPortFeatureFlags(..)
                              , OfpSwitchConfig(..), OfpSwitchCfgFlags(..)
                              , MACAddr
                              , ofCapabilities, ofStateFlags, ofConfigFlags, ofFeatureFlags
                              , openflow_1_0
                              ) where
 
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Set as S
import Data.Bits

openflow_1_0 :: Word8
openflow_1_0 = 0x01

-- TODO: replace Data.Set to something more effective for bitmaps

type MACAddr = Word64

data OfpHeader = OfpHeader { ofp_hdr_version :: Word8
                           , ofp_hdr_type    :: OfpType 
                           , ofp_hdr_length  :: Word16
                           , ofp_hdr_xid     :: Word32
                           }

data OfpMessage = OfpMessage { ofp_header  :: OfpHeader
                             , ofp_data    :: OfpMessageData
                             }

data OfpMessageData =   OfpMessageRaw BS.ByteString
                      | OfpEchoRequest BS.ByteString
                      | OfpEchoReply BS.ByteString
                      | OfpFeaturesRequest
                      | OfpFeatureReply OfpSwitchFeatures
                      | OfpSetConfig OfpSwitchConfig
                      | OfpHello
                      | OfpPacketOut BS.ByteString -- FIXME: implement real data type
                      | OfpUnsupported BS.ByteString

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
                                           , ofp_n_tables     :: Word8
                                           , ofp_capabilities :: S.Set OfpCapabilities
                                           , ofp_ports        :: [OfpPhyPort]
                                           } deriving (Show)

data OfpCapabilities =   OFPC_FLOW_STATS             --  Flow statistics
                       | OFPC_TABLE_STATS            --  Table statistics
                       | OFPC_PORT_STATS             --  Port statistics
                       | OFPC_STP                    --  802.1d spanning tree
                       | OFPC_RESERVED               --  Reserved, must be zero
                       | OFPC_IP_REASM               --  Can reassemble IP fragments
                       | OFPC_QUEUE_STATS            --  Queue statistics
                       | OFPC_ARP_MATCH_IP           --  Match IP addresses in ARP pkts
                       deriving (Eq, Ord, Enum, Show)

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
                    deriving(Eq, Ord, Enum, Show)

data OfpSwitchConfig = OfpSwitchConfig { ofp_switch_cfg_flags :: S.Set OfpSwitchCfgFlags
                                       , ofp_switch_cfg_miss_send_len :: Word16
                                       }

data OfpSwitchCfgFlags = OFPC_FRAG_NORMAL -- No special handling for fragments 
                       | OFPC_FRAG_DROP   -- Drop fragments
                       | OFPC_FRAG_REASM  -- Reassemble (only if OFPC_IP_REASM set)
                       | OFPC_FRAG_MASK
                       deriving (Eq, Ord, Enum, Show)

data OfpPhyPort = OfpPhyPort { ofp_port_no         :: Word16
                             , ofp_port_hw_addr    :: MACAddr
                             , ofp_port_name       :: BS.ByteString
                             , ofp_port_config     :: S.Set OfpPortConfigFlags
                             , ofp_port_state      :: S.Set OfpPortStateFlags
                             , ofp_port_current    :: S.Set OfpPortFeatureFlags
                             , ofp_port_advertised :: S.Set OfpPortFeatureFlags
                             , ofp_port_supported  :: S.Set OfpPortFeatureFlags
                             , ofp_port_peer       :: S.Set OfpPortFeatureFlags
                             } deriving (Show)

data OfpPortConfigFlags =   OFPPC_PORT_DOWN     -- Port is administratively down                        
                          | OFPPC_NO_STP        -- Disable 802.1D spanning tree on port
                          | OFPPC_NO_RECV       -- Drop all packets except 802.1D spanning tree packets
                          | OFPPC_NO_RECV_STP   -- Drop received 802.1D STP packets
                          | OFPPC_NO_FLOOD      -- Do not include this port when flooding
                          | OFPPC_NO_FWD        -- Drop packets forwarded to port
                          | OFPPC_NO_PACKET_IN  -- Do not send packet-in msgs for port
                          deriving(Eq, Ord, Enum, Show)

data OfpPortStateFlags =   OFPPS_LINK_DOWN    -- No physical link present
                         | OFPPS_STP_LISTEN   -- Not learning or relaying frames
                         | OFPPS_STP_LEARN    -- Learning but not relaying frames
                         | OFPPS_STP_FORWARD  -- Learning and relaying frames
                         | OFPPS_STP_BLOCK    -- Not part of spanning tree
                         | OFPPS_STP_MASK     -- Bit mask for OFPPS_STP_* values
                         deriving (Eq, Ord, Enum, Show)

data OfpPortFeatureFlags =   OFPPF_10MB_HD    --  10 Mb half-duplex rate support
                           | OFPPF_10MB_FD    --  10 Mb full-duplex rate support
                           | OFPPF_100MB_HD   --  100 Mb half-duplex rate support
                           | OFPPF_100MB_FD   --  100 Mb full-duplex rate support
                           | OFPPF_1GB_HD     --  1 Gb half-duplex rate support
                           | OFPPF_1GB_FD     --  1 Gb full-duplex rate support
                           | OFPPF_10GB_FD    --  10 Gb full-duplex rate support
                           | OFPPF_COPPER     --  Copper medium
                           | OFPPF_FIBER      --  Fiber medium
                           | OFPPF_AUTONEG    --  Auto-negotiation
                           | OFPPF_PAUSE      --  Pause
                           | OFPPF_PAUSE_ASYM --  Asymmetric pause
                           deriving (Eq, Ord, Enum, Show)

ofCapabilities OFPC_FLOW_STATS     = 1 `shiftL` 0
ofCapabilities OFPC_TABLE_STATS    = 1 `shiftL` 1
ofCapabilities OFPC_PORT_STATS     = 1 `shiftL` 2
ofCapabilities OFPC_STP            = 1 `shiftL` 3
ofCapabilities OFPC_RESERVED       = 1 `shiftL` 4
ofCapabilities OFPC_IP_REASM       = 1 `shiftL` 5
ofCapabilities OFPC_QUEUE_STATS    = 1 `shiftL` 6
ofCapabilities OFPC_ARP_MATCH_IP   = 1 `shiftL` 7

ofConfigFlags   OFPPC_PORT_DOWN    = 1 `shiftL` 0
ofConfigFlags   OFPPC_NO_STP       = 1 `shiftL` 1
ofConfigFlags   OFPPC_NO_RECV      = 1 `shiftL` 2
ofConfigFlags   OFPPC_NO_RECV_STP  = 1 `shiftL` 3
ofConfigFlags   OFPPC_NO_FLOOD     = 1 `shiftL` 4
ofConfigFlags   OFPPC_NO_FWD       = 1 `shiftL` 5
ofConfigFlags   OFPPC_NO_PACKET_IN = 1 `shiftL` 6

ofStateFlags   OFPPS_LINK_DOWN   = 1 `shiftL` 0
ofStateFlags   OFPPS_STP_LISTEN  = 0 `shiftL` 8
ofStateFlags   OFPPS_STP_LEARN   = 1 `shiftL` 8
ofStateFlags   OFPPS_STP_FORWARD = 2 `shiftL` 8
ofStateFlags   OFPPS_STP_BLOCK   = 3 `shiftL` 8
ofStateFlags   OFPPS_STP_MASK    = 3 `shiftL` 8

ofFeatureFlags   OFPPF_10MB_HD    = 1 `shiftL` 0
ofFeatureFlags   OFPPF_10MB_FD    = 1 `shiftL` 1
ofFeatureFlags   OFPPF_100MB_HD   = 1 `shiftL` 2
ofFeatureFlags   OFPPF_100MB_FD   = 1 `shiftL` 3
ofFeatureFlags   OFPPF_1GB_HD     = 1 `shiftL` 4
ofFeatureFlags   OFPPF_1GB_FD     = 1 `shiftL` 5
ofFeatureFlags   OFPPF_10GB_FD    = 1 `shiftL` 6
ofFeatureFlags   OFPPF_COPPER     = 1 `shiftL` 7
ofFeatureFlags   OFPPF_FIBER      = 1 `shiftL` 8
ofFeatureFlags   OFPPF_AUTONEG    = 1 `shiftL` 9
ofFeatureFlags   OFPPF_PAUSE      = 1 `shiftL` 10
ofFeatureFlags   OFPPF_PAUSE_ASYM = 1 `shiftL` 11


