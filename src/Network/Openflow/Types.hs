{-# Language BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Openflow.Types ( OfpHeader(..), OfpType(..), OfpMessage(..), OfpMessageData(..)
                              , OfpCapabilities(..), OfpSwitchFeatures(..), OfpPhyPort(..)
                              , OfpPortConfigFlags(..), OfpPortStateFlags(..), OfpPortFeatureFlags(..)
                              , OfpActionType(..)
                              , OfpSwitchConfig(..), OfpSwitchCfgFlags(..), OfpError(..)
                              , OfpErrorType(..), OfpHelloFailedCode(..), OfpBadActionCode(..)
                              , OfpBadRequestCode(..), OfpFlowModFailedCode(..), OfpPortModFailedCode(..)
                              , OfpQueueOpFailedCode(..), OfpPacketIn(..), OfpPacketInReason(..)
                              , OfpPacketOutData(..), OfpStatsType(..), 
                              -- * flow mod
                              OfpFlowModData(..), OfpFlowModCommand(..), OfpMatch(..)
                              , OfpFlowModFlag(..), ofpf_SEND_FLOW_REM, ofpf_CHECK_OVERLAP, ofpf_EMERG
                              -- * other
                              , MACAddr
                              , ofCapabilities, ofStateFlags, ofConfigFlags, ofFeatureFlags, ofErrorType
                              , ofCapabilitiesUnflag, ofStateUnflag, ofConfigUnflag, ofFeatureUnflag
                              , ofActionType, ofActionTypeUnflag
                              , ofErrorCode
                              , openflow_1_0
                              , defaultSwitchConfig
                              , listToFlags, flagsToList
                              ) where

import Control.Applicative
import Network.Openflow.Ethernet.Types (MACAddr, IPv4Addr)
import Network.Openflow.StrictPut
import Data.Binary
import Data.Binary.Get
import Data.Default
import Data.Word
import qualified Data.ByteString as BS
import Data.Bits

openflow_1_0 :: Word8
openflow_1_0 = 0x01

-- TODO: split to several files
-- TODO: replace Data.Set to something more effective for bitmaps

type FlagSet = Word32
type Flag = Word32

data OfpHeader = OfpHeader { ofp_hdr_version :: !Word8
                           , ofp_hdr_type    :: !OfpType 
                           , ofp_hdr_length  :: !Word16
                           , ofp_hdr_xid     :: !Word32
                           }
                           deriving (Show)

instance Default OfpHeader where def = OfpHeader def def def def

data OfpMessage = OfpMessage { ofp_header  :: !OfpHeader
                             , ofp_data    :: !OfpMessageData
                             }
                             deriving (Show)

instance Default OfpMessage where
  def = OfpMessage def def

data OfpMessageData =   OfpMessageRaw       !BS.ByteString
                      | OfpEchoRequest      !BS.ByteString
                      | OfpEchoReply        !BS.ByteString
                      | OfpFeaturesRequest
                      | OfpFeatureReply     !OfpSwitchFeatures
                      | OfpSetConfig        !OfpSwitchConfig
                      | OfpGetConfigRequest
                      | OfpGetConfigReply   !OfpSwitchConfig
                      | OfpHello 
                      | OfpEmptyReply
                      | OfpPacketOut        !OfpPacketOutData  -- FIXME: implement real data type
                      | OfpVendor           !BS.ByteString    -- WTF?
                      | OfpErrorReply       !OfpError
                      | OfpPacketInReply    !OfpPacketIn
                      | OfpStatsRequest     !OfpStatsType
                      | OfpStatsReply
                      | OfpFlowMod          !OfpFlowModData 
                      | OfpUnsupported      !BS.ByteString
                      deriving (Show)


instance Default OfpMessageData where
  def = OfpEmptyReply

data OfpType  = 
    -- Immutable messages
      OFPT_HELLO               -- Symmetric message
    | OFPT_ERROR               -- Symmetric message
    | OFPT_ECHO_REQUEST        -- Symmetric message
    | OFPT_ECHO_REPLY          -- Symmetric message
    | OFPT_VENDOR              -- Symmetric message

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
    | OFPT_PORT_MOD            -- Controller/switch message

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

instance Default OfpType where def = OFPT_HELLO

data OfpSwitchFeatures = OfpSwitchFeatures { ofp_datapath_id  :: !Word64
                                           , ofp_n_buffers    :: !Word32
                                           , ofp_n_tables     :: !Word8
                                           , ofp_capabilities :: !FlagSet
                                           , ofp_actions      :: !FlagSet
                                           , ofp_ports        :: ![OfpPhyPort]
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

data OfpActionType =   OFPAT_OUTPUT        -- Output to switch port
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

data OfpSwitchConfig = OfpSwitchConfig { ofp_switch_cfg_flags         :: !OfpSwitchCfgFlags
                                       , ofp_switch_cfg_miss_send_len :: !Word16
                                       }
                                       deriving (Show)

data OfpSwitchCfgFlags = OFPC_FRAG_NORMAL -- No special handling for fragments 
                       | OFPC_FRAG_DROP   -- Drop fragments
                       | OFPC_FRAG_REASM  -- Reassemble (only if OFPC_IP_REASM set)
                       | OFPC_FRAG_MASK
                       deriving (Eq, Ord, Enum, Show)

defaultSwitchConfig :: OfpSwitchConfig
defaultSwitchConfig = OfpSwitchConfig OFPC_FRAG_NORMAL 128

data OfpPhyPort = OfpPhyPort { ofp_port_no         :: Word16
                             , ofp_port_hw_addr    :: MACAddr
                             , ofp_port_name       :: BS.ByteString
                             , ofp_port_config     :: FlagSet --S.Set OfpPortConfigFlags
                             , ofp_port_state      :: FlagSet --S.Set OfpPortStateFlags
                             , ofp_port_current    :: FlagSet --S.Set OfpPortFeatureFlags
                             , ofp_port_advertised :: FlagSet --S.Set OfpPortFeatureFlags
                             , ofp_port_supported  :: FlagSet --S.Set OfpPortFeatureFlags
                             , ofp_port_peer       :: FlagSet --S.Set OfpPortFeatureFlags
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

ofActionType :: OfpActionType -> Flag
ofActionType OFPAT_VENDOR = 0xFFFF
ofActionType x = 1 `shiftL` (fromEnum x)

-- TODO: try to use template haskell

ofActionTypeUnflag :: Flag -> OfpActionType
ofActionTypeUnflag 0xFFFF = OFPAT_VENDOR
ofActionTypeUnflag flag
        | (1 `shiftL`  0) == flag = (toEnum  0)
        | (1 `shiftL`  1) == flag = (toEnum  1)
        | (1 `shiftL`  2) == flag = (toEnum  2)
        | (1 `shiftL`  3) == flag = (toEnum  3)
        | (1 `shiftL`  4) == flag = (toEnum  4)
        | (1 `shiftL`  5) == flag = (toEnum  5)
        | (1 `shiftL`  6) == flag = (toEnum  6)
        | (1 `shiftL`  7) == flag = (toEnum  7)
        | (1 `shiftL`  8) == flag = (toEnum  8)
        | (1 `shiftL`  9) == flag = (toEnum  9)
        | (1 `shiftL` 10) == flag = (toEnum 10)
        | (1 `shiftL` 11) == flag = (toEnum 11)
        | (1 `shiftL` 12) == flag = (toEnum 12)
        | (1 `shiftL` 13) == flag = (toEnum 13)
        | otherwise               = error "action type is not supported"

ofCapabilities :: (Num a, Bits a) => OfpCapabilities -> a
ofCapabilities OFPC_FLOW_STATS     = 1 `shiftL` 0
ofCapabilities OFPC_TABLE_STATS    = 1 `shiftL` 1
ofCapabilities OFPC_PORT_STATS     = 1 `shiftL` 2
ofCapabilities OFPC_STP            = 1 `shiftL` 3
ofCapabilities OFPC_RESERVED       = 1 `shiftL` 4
ofCapabilities OFPC_IP_REASM       = 1 `shiftL` 5
ofCapabilities OFPC_QUEUE_STATS    = 1 `shiftL` 6
ofCapabilities OFPC_ARP_MATCH_IP   = 1 `shiftL` 7

ofCapabilitiesUnflag :: Flag -> OfpCapabilities
ofCapabilitiesUnflag flag
        | (1 `shiftL`  0) == flag = (toEnum  0)
        | (1 `shiftL`  1) == flag = (toEnum  1)
        | (1 `shiftL`  2) == flag = (toEnum  2)
        | (1 `shiftL`  3) == flag = (toEnum  3)
        | (1 `shiftL`  4) == flag = (toEnum  4)
        | (1 `shiftL`  5) == flag = (toEnum  5)
        | (1 `shiftL`  6) == flag = (toEnum  6)
        | (1 `shiftL`  7) == flag = (toEnum  7)
        | otherwise               = error "capability is not supported"

ofConfigFlags :: (Num a, Bits a) => OfpPortConfigFlags -> a
ofConfigFlags   OFPPC_PORT_DOWN    = 1 `shiftL` 0
ofConfigFlags   OFPPC_NO_STP       = 1 `shiftL` 1
ofConfigFlags   OFPPC_NO_RECV      = 1 `shiftL` 2
ofConfigFlags   OFPPC_NO_RECV_STP  = 1 `shiftL` 3
ofConfigFlags   OFPPC_NO_FLOOD     = 1 `shiftL` 4
ofConfigFlags   OFPPC_NO_FWD       = 1 `shiftL` 5
ofConfigFlags   OFPPC_NO_PACKET_IN = 1 `shiftL` 6

ofConfigUnflag :: Flag -> OfpSwitchCfgFlags
ofConfigUnflag flag
        | (1 `shiftL`  0) == flag = (toEnum  0)
        | (1 `shiftL`  1) == flag = (toEnum  1)
        | (1 `shiftL`  2) == flag = (toEnum  2)
        | (1 `shiftL`  3) == flag = (toEnum  3)
        | (1 `shiftL`  4) == flag = (toEnum  4)
        | (1 `shiftL`  5) == flag = (toEnum  5)
        | (1 `shiftL`  6) == flag = (toEnum  6)
        | otherwise               = error "config flag is not supported"

ofStateFlags :: (Num a, Bits a) => OfpPortStateFlags -> a
ofStateFlags   OFPPS_LINK_DOWN   = 1 `shiftL` 0
ofStateFlags   OFPPS_STP_LISTEN  = 0 `shiftL` 8
ofStateFlags   OFPPS_STP_LEARN   = 1 `shiftL` 8
ofStateFlags   OFPPS_STP_FORWARD = 2 `shiftL` 8
ofStateFlags   OFPPS_STP_BLOCK   = 3 `shiftL` 8
ofStateFlags   OFPPS_STP_MASK    = 3 `shiftL` 8

ofStateUnflag :: Flag -> OfpPortStateFlags
ofStateUnflag flag
        | (1 `shiftL`  0) == flag = (toEnum  0)
        | (0 `shiftL`  0) == flag = (toEnum  1)
        | (1 `shiftL`  8) == flag = (toEnum  2)
        | (2 `shiftL`  8) == flag = (toEnum  3)
        | (3 `shiftL`  8) == flag = (toEnum  4)
        | (3 `shiftL`  8) == flag = (toEnum  5)
        | otherwise               = error "state flag is not supported" -- TODO use unsupported flag in enum?

ofFeatureFlags ::  (Num a, Bits a) => OfpPortFeatureFlags -> a
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

ofFeatureUnflag :: Flag -> OfpPortFeatureFlags
ofFeatureUnflag flag
        | (1 `shiftL`  0) == flag = (toEnum  0)
        | (1 `shiftL`  1) == flag = (toEnum  1)
        | (1 `shiftL`  2) == flag = (toEnum  2)
        | (1 `shiftL`  3) == flag = (toEnum  3)
        | (1 `shiftL`  4) == flag = (toEnum  4)
        | (1 `shiftL`  5) == flag = (toEnum  5)
        | (1 `shiftL`  6) == flag = (toEnum  6)
        | (1 `shiftL`  7) == flag = (toEnum  7)
        | (1 `shiftL`  8) == flag = (toEnum  8)
        | (1 `shiftL`  9) == flag = (toEnum  9)
        | (1 `shiftL` 10) == flag = (toEnum 10)
        | (1 `shiftL` 11) == flag = (toEnum 11)
        | otherwise               = error "no such flag"

data OfpError = OfpError { ofp_error_type :: OfpErrorType
                         , ofp_error_data :: BS.ByteString
                         }
                         deriving (Show)

data OfpErrorType =   OFPET_HELLO_FAILED OfpHelloFailedCode      -- Hello protocol failed
                    | OFPET_BAD_REQUEST  OfpBadRequestCode       -- Request was not understood
                    | OFPET_BAD_ACTION   OfpBadActionCode        -- Error in action description
                    | OFPET_FLOW_MOD_FAILED OfpFlowModFailedCode -- Problem modifying flow entry
                    | OFPET_PORT_MOD_FAILED OfpPortModFailedCode -- Port mod request failed
                    | OFPET_QUEUE_OP_FAILED OfpQueueOpFailedCode -- Queue operation failed
                    deriving (Ord, Eq, Show)

data OfpHelloFailedCode = OFPHFC_INCOMPATIBLE -- No compatible version
                        | OFPHFC_EPERM        -- Permissions error
                        deriving (Ord, Eq, Enum, Show)

data OfpBadRequestCode =   OFPBRC_BAD_VERSION         --  ofp_header.version not supported
                         | OFPBRC_BAD_TYPE            --  ofp_header.type not supported
                         | OFPBRC_BAD_STAT            --  ofp_stats_request.type not supported
                         | OFPBRC_BAD_VENDOR          --  Vendor not supported (in ofp_vendor_header or ofp_stats_request or ofp_stats_reply)
                         | OFPBRC_BAD_SUBTYPE         --  Vendor subtype not supported
                         | OFPBRC_EPERM               --  Permissions error
                         | OFPBRC_BAD_LEN             --  Wrong request length for type
                         | OFPBRC_BUFFER_EMPTY        --  Specified buffer has already been used
                         | OFPBRC_BUFFER_UNKNOWN       --  Specified buffer does not exist
                         deriving (Ord, Eq, Enum, Show)


data OfpBadActionCode =  OFPBAC_BAD_TYPE           --  Unknown action type
                       | OFPBAC_BAD_LEN            --  Length problem in actions
                       | OFPBAC_BAD_VENDOR         --  Unknown vendor id specified
                       | OFPBAC_BAD_VENDOR_TYPE    --  Unknown action type for vendor id
                       | OFPBAC_BAD_OUT_PORT       --  Problem validating output action
                       | OFPBAC_BAD_ARGUMENT       --  Bad action argument
                       | OFPBAC_EPERM              --  Permissions error
                       | OFPBAC_TOO_MANY           --  Can't handle this many actions
                       | OFPBAC_BAD_QUEUE          --  Problem validating output queue
                       deriving (Ord, Eq, Enum, Show)


data OfpFlowModFailedCode =   OFPFMFC_ALL_TABLES_FULL    --  Flow not added because of full tables
                            | OFPFMFC_OVERLAP            --  Attempted to add overlapping flow with CHECK_OVERLAP flag set
                            | OFPFMFC_EPERM              --  Permissions error
                            | OFPFMFC_BAD_EMERG_TIMEOUT  --  Flow not added because of non-zero idle/hard timeout
                            | OFPFMFC_BAD_COMMAND        --  Unknown command
                            | OFPFMFC_UNSUPPORTED        --  Unsupported action list - cannot process in the order specified
                            deriving (Ord, Eq, Enum, Show)

data OfpPortModFailedCode =   OFPPMFC_BAD_PORT            --  Specified port does not exist
                            | OFPPMFC_BAD_HW_ADDR         --  Specified hardware address is wrong
                            deriving (Ord, Eq, Enum, Show)

data OfpQueueOpFailedCode =   OFPQOFC_BAD_PORT           --  Invalid port (or port does not exist)
                            | OFPQOFC_BAD_QUEUE          --  Queue does not exist.
                            | OFPQOFC_EPERM              --  Permissions error
                            deriving (Ord, Eq, Enum, Show)

ofErrorType :: OfpErrorType -> Int
ofErrorType (OFPET_HELLO_FAILED _)     = 0
ofErrorType (OFPET_BAD_REQUEST  _)     = 1
ofErrorType (OFPET_BAD_ACTION   _)     = 2
ofErrorType (OFPET_FLOW_MOD_FAILED _)  = 3
ofErrorType (OFPET_PORT_MOD_FAILED _)  = 4
ofErrorType (OFPET_QUEUE_OP_FAILED _)  = 5

ofErrorCode :: OfpErrorType -> Int
ofErrorCode (OFPET_HELLO_FAILED x)     = fromEnum x
ofErrorCode (OFPET_BAD_REQUEST  x)     = fromEnum x
ofErrorCode (OFPET_BAD_ACTION   x)     = fromEnum x
ofErrorCode (OFPET_FLOW_MOD_FAILED x)  = fromEnum x
ofErrorCode (OFPET_PORT_MOD_FAILED x)  = fromEnum x
ofErrorCode (OFPET_QUEUE_OP_FAILED x)  = fromEnum x

-- | PacketIn data
data OfpPacketIn = OfpPacketIn { ofp_pkt_in_buffer_id :: !Word32                -- ^ switch buffer id
                               , ofp_pkt_in_in_port   :: !Word16                -- ^ switch port that received message
                               , ofp_pkt_in_reason    :: !OfpPacketInReason     -- ^ reason for query
                               , ofp_pkt_in_data      :: !(PutM ())             -- ^ data
                               }

instance Default OfpPacketIn where def = OfpPacketIn def def def (return ())

instance Show OfpPacketIn where
    show (OfpPacketIn a b c _) = "OfpPacketIn "++show a++" "++show b++" "++show c++"<data>"

data OfpPacketInReason = OFPR_NO_MATCH | OFPR_ACTION
                         deriving (Eq, Ord, Enum, Show)

instance Default OfpPacketInReason where def = OFPR_NO_MATCH


data OfpFlowModData = OfpFlowModData 
      { ofp_flow_mod_match         :: !OfpMatch             -- ^ fields to match
      , ofp_flow_mod_cookie        :: !Word64               -- ^ opaque controller-issued identifier
      , ofp_flow_mod_command       :: !OfpFlowModCommand    
      , ofp_flow_mod_idle_timeout  :: !Word16               -- ^ idle time before discarding (seconds)
      , ofp_flow_mod_hard_timeout  :: !Word16               -- ^ max time before discarding  (seconds)
      , ofp_flow_mod_priority      :: !Word16               -- ^ proprity level of flow entry
      , ofp_flow_mod_buffer_id     :: !Word32               -- ^ buffered packet to apply to (or -1)
      , ofp_flow_mod_out_port      :: !Word16
      , ofp_flow_mod_flags         :: !OfpFlowModFlag
      , ofp_flow_mod_action_header :: !BS.ByteString
      }
      deriving (Show)

instance Binary OfpFlowModData where
  put x = error "not yet implemented" -- TODO: implement? 
  get = OfpFlowModData <$> get            -- match
                       <*> getWord64be    -- cookie
                       <*> get            -- command
                       <*> getWord16be    -- idle
                       <*> getWord16be    -- hard
                       <*> getWord16be    -- prio
                       <*> getWord32be    -- bufferid
                       <*> getWord16be    -- out-port
                       <*> get            -- mod flag
                       <*> pure BS.empty  -- TODO: fixme

data OfpFlowModCommand = OFPFC_ADD                -- ^ new flow
                       | OFPFC_MODIFY             -- ^ modify all matching flows
                       | OFPFC_MODIFY_STRICT      -- ^ modify entry strictly matching wildcard
                       | OFPFC_DELETE             -- ^ delete all matching flows
                       | OFPFC_DELETE_STRICT      -- ^ strictly matching wildcards and priority
                       deriving (Eq,Enum,Show)

instance Binary OfpFlowModCommand where
    put = error "not yet implemented" -- putWord16be . fromIntegral . fromEnum
    get = toEnum . fromIntegral <$> getWord16be


newtype OfpFlowModFlag = OfpFlowModFlag {unOfpFlowModFlag :: Word16}
                         deriving (Eq,Show)


ofpf_SEND_FLOW_REM , ofpf_CHECK_OVERLAP, ofpf_EMERG :: OfpFlowModFlag
ofpf_SEND_FLOW_REM = OfpFlowModFlag (1`shiftL`0)
ofpf_CHECK_OVERLAP = OfpFlowModFlag (1`shiftL`1)
ofpf_EMERG         = OfpFlowModFlag (1`shiftL`2)

instance Binary OfpFlowModFlag where
  put = error "not yet impemented"
  get = OfpFlowModFlag <$> getWord16be

data OfpMatch = OfpMatch 
      { ofp_match_wildcards   :: !Word32    -- ^ wildcard fields
      , ofp_match_in_ports    :: !Word16    -- ^ input switch port
      , ofp_match_dl_src      :: !MACAddr   -- ^ ethernet source address
      , ofp_match_dl_dst      :: !MACAddr   -- ^ ethernet dest address
      , ofp_match_dl_vlan     :: !Word16    -- ^ input VLAN id
      , ofp_match_dl_vlan_pcp :: !Word8     -- ^ VLAN priority
      , ofp_match_dl_type     :: !Word16    -- ^ ethernet frame type
      , ofp_match_nw_tos      :: !Word8     -- ^ IP ToS
      , ofp_match_nw_proto    :: !Word8     -- ^ IP protocol or lower 8 bits ARP OP code
      , ofp_match_nw_src      :: IPv4Addr   -- ^ IP source address
      , ofp_match_nw_dst      :: IPv4Addr   -- ^ IP destination address
      , ofp_match_tp_src      :: !Word16    -- ^ TCP/UDP source port
      , ofp_match_tp_dst      :: !Word16    -- ^ TCP/UDP dest port
      }
      deriving (Show)

instance Binary OfpMatch where
  put = error "not yet implemented" -- TODO implement
  get = OfpMatch <$> getWord32be 
                 <*> getWord16be
                 <*> (unBMac <$> get)
                 <*> (unBMac <$> get)
                 <*> getWord16be
                 <*> getWord8
                 <*  getWord8
                 <*> getWord16be
                 <*> getWord8
                 <*> getWord8
                 <*  getWord16be
                 <*> getWord32be
                 <*> getWord32be
                 <*> getWord16be
                 <*> getWord16be

newtype BMac = BMac {unBMac :: MACAddr}

instance Binary BMac where
  put = error "not yet implemented"
  get = do x <- fromIntegral <$> getWord16be
           y <- fromIntegral <$> getWord32be
           return . BMac $ (x `shiftL` 32) + y

data OfpPacketOutData = OfpPacketOutData { ofp_pkt_out_buffer_id :: !Word32
                                         , ofp_pkt_out_in_port   :: !Word16
                                         -- TODO: implement rest of message
                                         }
                                         deriving (Show, Eq)

data OfpStatsType =   OFPST_DESC
                    | OFPST_FLOW
                    | OFPST_AGGREGATE
                    | OFPST_TABLE
                    | OFPST_PORT
                    | OFPST_QUEUE
                    | OFPST_VENDOR
                    deriving (Eq, Ord, Show)

instance Enum OfpStatsType where
  fromEnum OFPST_DESC       = 0 
  fromEnum OFPST_FLOW       = 1
  fromEnum OFPST_AGGREGATE  = 2
  fromEnum OFPST_TABLE      = 3
  fromEnum OFPST_PORT       = 4
  fromEnum OFPST_QUEUE      = 5
  fromEnum OFPST_VENDOR     = 0xFFFF

  toEnum 0      = OFPST_DESC
  toEnum 1      = OFPST_FLOW
  toEnum 2      = OFPST_AGGREGATE
  toEnum 3      = OFPST_TABLE
  toEnum 4      = OFPST_PORT
  toEnum 5      = OFPST_QUEUE
  toEnum 0xFFFF = OFPST_VENDOR
  toEnum _      = error "OfpStatsType is not supported"

listToFlags :: (a -> Flag) -> [a] -> FlagSet
listToFlags f = foldl (\acc val -> acc .|. (f val) ) 0

flagsToList :: (Flag -> a) -> FlagSet -> [a]
flagsToList f set = testFlag set 1 []
    where --TODO try without recursion (find fold/map functions for bitsets)
        testFlag set' bitn list = 
          let mask = bit bitn in
          if testBit set' bitn 
            then testFlag (clearBit set' bitn) (bitn+1) ( (f mask):list )
            else 
              if set == 0
                then list
                else testFlag set' (bitn+1) list
