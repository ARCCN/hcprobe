{-# LANGUAGE TemplateHaskell #-}
module HCProbe.EDSL.PacketGeneration
  ( -- * openflow functions
    putOFMessage
    -- * header
  , putOFHeader
  , putHdrVersion
  , putHdrType
  , putPacketLength
  , putHdrXid
    -- * payload
  , putOFData
  , putRaw
  -- * OfpPacketIn
  , putPacketIn
  , putPacketInBufferId
  , putPacketInPort
  , putPacketInReason
  , putPacketInData
  -- * OfpPortStatusData
  , putPortStatus
  , putPortStatusReason
  , putPortStatusPortDirect
  , putPortStatusPort
  , putFlowRemoved
    -- * OfpPhyPort
  , putPortNo
  , putPortHwAddr
  , putPortName
  , putPortConfig
  , putPortState
  , putPortCurrent
  , putPortAdvertised
  , putPortSupported
  , putPortPeer
    -- * OfpMatch
  , putMatchWildcards
  , putMatchInPorts
  , putMatchDlSrc
  , putMatchDlDst
  , putMatchDlVlan
  , putMatchDlVlanPcp
  , putMatchDlType
  , putMatchNwTos
  , putMatchNwProto
  , putMatchNwSrc
  , putMatchNwDst
  , putMatchTpSrc
  , putMatchTpDst
    -- * OfpFlowRemovedData
  , putFlowRemovedCookie
  , putFlowRemovedPriority
  , putFlowRemovedReason
  , putFlowRemovedTableId
  , putFlowRemovedDurationSec
  , putFlowRemovedDurationNsec
  , putFlowRemovedIdleTimeout
  , putFlowRemovedHardTimeout
  , putFlowRemovedPacketCount
  , putFlowRemovedByteCount
  , putFlowRemovedMatch
  , putFlowRemovedMatchDirect
  -- * OfpErrorMessage
  , putErrorMessage
  , putErrorType
  , putErrorData
  -- * OfpFeaturesReply
  , putFeaturesReply
  , putSwitchFeaturesDatapathId
  , putSwitchFeaturesNBuffers
  , putSwitchFeaturesNTables
  , putSwitchFeaturesCapabilities
  , putSwitchFeaturesActions
  , putSwitchFeaturesPorts
  -- * OfpGetConfigReply
  , putGetConfigReply
  , putSwitchCfgFlags
  , putSwitchCfgMissSendLen
  ) where

import Control.Monad.Writer
import Data.ByteString (ByteString)
import Data.Default
import Data.Word

import Network.Openflow.Types
import Network.Openflow.StrictPut

import HCProbe.EDSL.TH


-- FIXME use buffer or such stuff
putOFMessage :: Writer (Endo OfpMessage) a -> OfpMessage
putOFMessage w = appEndo (execWriter w) def

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
putPacketLength :: Word16 -> Writer (Endo OfpHeader) ()
putPacketLength l = tell . Endo $ \h -> h{ofp_packet_length = Just l}

-- | Add xid
putHdrXid :: Word32 -> Writer (Endo OfpHeader) ()
putHdrXid x = tell . Endo $ \h -> h{ofp_hdr_xid = x}

putRaw :: ByteString -> Writer (Endo OfpMessage) ()
putRaw bs = tell . Endo $ \m -> m{ofp_data = OfpMessageRaw bs}

putOFData :: OfpMessageData -> Writer (Endo OfpMessage) ()
putOFData m = tell . Endo $ \x -> x {ofp_data = m}

putPacketIn :: Writer (Endo OfpPacketIn) a -> Writer (Endo OfpMessage) ()
putPacketIn w = tell . Endo $ \m -> m{ofp_data = OfpPacketInReply (appEndo (execWriter w) def)}

putPacketInReason :: OfpPacketInReason -> Writer (Endo OfpPacketIn) ()
putPacketInReason r = tell . Endo $ \m -> m{ofp_pkt_in_reason = r}

putPacketInPort :: Word16 -> Writer (Endo OfpPacketIn) ()
putPacketInPort p = tell . Endo $ \m -> m{ofp_pkt_in_in_port = p}

putPacketInBufferId :: Word32 -> Writer (Endo OfpPacketIn) ()
putPacketInBufferId i = tell . Endo $ \m -> m{ofp_pkt_in_buffer_id = i}

putPacketInData :: Put -> Writer (Endo OfpPacketIn) ()
putPacketInData p = tell . Endo $ \m -> m{ofp_pkt_in_data = p}

putPortStatus :: Writer (Endo OfpPortStatusData) a -> Writer (Endo OfpMessage) ()
putPortStatus w = tell . Endo $ \m -> m{ofp_data = OfpPortStatus (appEndo (execWriter w) def)}

putPortStatusReason :: OfpPortReason -> Writer (Endo OfpPortStatusData) ()
putPortStatusReason r = tell . Endo $ \m -> m{opt_port_status_reason = r}

putPortStatusPortDirect :: OfpPhyPort -> Writer (Endo OfpPortStatusData) ()
putPortStatusPortDirect p = tell . Endo $ \m -> m{opt_port_status_desc = p}

putPortStatusPort :: Writer (Endo OfpPhyPort) a -> Writer (Endo OfpPortStatusData) ()
putPortStatusPort w = tell . Endo $ \m -> m{opt_port_status_desc = appEndo (execWriter w) def}

$(generatePutters ''OfpPhyPort)

$(generatePutters ''OfpMatch)

putFlowRemoved :: Writer (Endo OfpFlowRemovedData) a -> Writer (Endo OfpMessage) ()
putFlowRemoved w = tell . Endo $ \m -> m{ofp_data = OfpFlowRemoved (appEndo (execWriter w) def)}

putFlowRemovedMatch :: Writer (Endo OfpMatch) a -> Writer (Endo OfpFlowRemovedData) ()
putFlowRemovedMatch w = tell . Endo $ \m -> m{ofp_flow_removed_match = appEndo (execWriter w) def}

$(generatePutters ''OfpFlowRemovedData)

putErrorMessage :: Writer (Endo OfpError) a -> Writer (Endo OfpMessage) ()
putErrorMessage w = tell . Endo $ \m -> m{ofp_data = OfpErrorReply (appEndo (execWriter w) def)}

putErrorType :: OfpErrorType -> Writer (Endo OfpError) ()
putErrorType t = tell . Endo $ \p -> p{ofp_error_type = t}

putErrorData :: ByteString -> Writer (Endo OfpError) ()
putErrorData d = tell . Endo $ \p -> p{ofp_error_data = d}

putFeaturesReply :: Writer (Endo OfpSwitchFeatures) a -> Writer (Endo OfpMessage) ()
putFeaturesReply w = tell . Endo $ \m -> m{ofp_data = OfpFeaturesReply (appEndo (execWriter w) def)}

$(generatePutters ''OfpSwitchFeatures)

putGetConfigReply :: Writer (Endo OfpSwitchConfig) a -> Writer (Endo OfpMessage) ()
putGetConfigReply w = tell . Endo $ \m -> m{ofp_data = OfpGetConfigReply (appEndo (execWriter w) def)}

$(generatePutters ''OfpSwitchConfig)
