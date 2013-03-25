module Network.Openflow.Messages ( ofpHelloRequest -- FIXME <- not needed
                                 , ofpParsePacket  -- FIXME <- not needed
                                 , parseMessageData
                                 , bsStrict
                                 , putMessage
                                 , header
                                 , featuresReply
                                 , echoReply
                                 , headReply
                                 , errorReply
                                 , getConfigReply
                                 , putOfpPort
                                 , putOfpPacketIn
                                 ) where

import Network.Openflow.Types
import Network.Openflow.Misc
import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Bits
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Control.Monad

import Debug.Trace

-- FIXME: rename ofpParse* to getOfp*


ofpHeaderLen = (8 + 8 + 16 + 32) `div` 8

ofpHelloRequest :: Word8 -> Word32 -> PutM ()
ofpHelloRequest v xid = putMessageHeader 0 h
  where h = OfpHeader { ofp_hdr_version = v
                      , ofp_hdr_type    = OFPT_HELLO
                      , ofp_hdr_length  = fromIntegral ofpHeaderLen
                      , ofp_hdr_xid     = xid
                      }


header :: Word8 -> Word32 -> OfpType -> OfpHeader
header v x t = OfpHeader v t (fromIntegral ofpHeaderLen) x

featuresReply ov sw xid = OfpMessage hdr feature_repl
  where hdr = header ov xid OFPT_FEATURES_REPLY
        feature_repl = OfpFeatureReply sw

echoReply ov payload xid = OfpMessage hdr (OfpEchoReply payload)
  where hdr = header ov xid OFPT_ECHO_REPLY       

headReply :: OfpHeader -> OfpType -> OfpMessage
headReply h t = OfpMessage newHead OfpEmptyReply
  where newHead = h {ofp_hdr_type = t, ofp_hdr_length = fromIntegral ofpHeaderLen}

errorReply h tp = OfpMessage newHead (OfpErrorReply tp)
  where newHead = h { ofp_hdr_type = OFPT_ERROR, ofp_hdr_length = fromIntegral ofpHeaderLen}

getConfigReply :: OfpHeader -> OfpSwitchConfig -> OfpMessage
getConfigReply hdr cfg = OfpMessage newHead (OfpGetConfigReply cfg)
  where newHead = hdr { ofp_hdr_type = OFPT_GET_CONFIG_REPLY }

packetIn = undefined

ofpParseHeader :: Get OfpHeader
ofpParseHeader = do
    v   <- getWord8
    tp  <- getWord8
    len <- getWord16be
    xid <- getWord32be
    -- FIXME: enum code overflow
    return $ OfpHeader v (toEnum (fromIntegral tp)) len xid

ofpParsePacket :: BS.ByteString -> Maybe (OfpMessage, BS.ByteString)
ofpParsePacket s = withResult $ flip runGet s $ do
  hdr <- ofpParseHeader
  let plen = (fromIntegral (ofp_hdr_length hdr)) - ofpHeaderLen
  bs  <- getByteString plen
  return $ OfpMessage hdr (OfpMessageRaw bs)
  where withResult (Left _, _)    = Nothing
        withResult (Right msg, rest) = Just (msg, rest)

parseMessageData :: OfpMessage -> Maybe OfpMessage

parseMessageData (OfpMessage hdr (OfpMessageRaw bs)) = parse (ofp_hdr_type hdr)
  where 
    parse OFPT_HELLO            = runParse (return OfpHello)
    parse OFPT_FEATURES_REQUEST = runParse (return OfpFeaturesRequest)
    parse OFPT_ECHO_REQUEST     = runParse (return (OfpEchoRequest bs))
    parse OFPT_SET_CONFIG       = runParse getOfpSetConfig
    parse OFPT_GET_CONFIG_REQUEST = runParse (return OfpGetConfigRequest)
    parse OFPT_PACKET_OUT       = runParse getPacketOut
    parse OFPT_VENDOR           = runParse (return (OfpVendor bs))
    parse _                     = runParse (return (OfpUnsupported bs))

    runParse fGet =
      case (runGet fGet bs) of
        (Left _, _)  -> Nothing
        (Right x, _) -> Just ((OfpMessage hdr) x)

parseMessageData x@(OfpMessage _ _) = Just x -- already parsed

getOfpSetConfig :: Get OfpMessageData
getOfpSetConfig = do 
  wFlags <- getWord16be
  wSendL <- getWord16be
                             -- FIXME: possible enum overflow
  return $ OfpSetConfig $ OfpSwitchConfig { ofp_switch_cfg_flags = toEnum (fromIntegral wFlags)
                                          , ofp_switch_cfg_miss_send_len = wSendL
                                          }
getPacketOut :: Get OfpMessageData
getPacketOut = do
  bid  <- getWord32be
  pid  <- getWord16be
  alen <- getWord16be
  skip (fromIntegral alen)
  return $ OfpPacketOut (OfpPacketOutData bid pid)

putMessage :: OfpMessage -> PutM ()
putMessage (OfpMessage h d) = putMessageHeader dataLen h >> putByteString dataS
  where dataS = bsStrict $ runPut (putMessageData d)
        dataLen = BS.length dataS

putMessageHeader :: Int -> OfpHeader -> PutM ()
putMessageHeader plen h = putWord8 version >> putWord8 tp >> putWord16be len >> putWord32be xid
  where version = ofp_hdr_version h
        tp      = (fromIntegral.fromEnum.ofp_hdr_type) h
        len     = ofp_hdr_length h + fromIntegral plen
        xid     = ofp_hdr_xid h

putMessageData :: OfpMessageData -> PutM ()
putMessageData OfpHello = return ()

putMessageData (OfpFeatureReply f) = do
  putWord64be (ofp_datapath_id f)
  putWord32be (ofp_n_buffers f)
  putWord8    (ofp_n_tables f)
  replicateM_ 3 (putWord8 0)
--  putWord32be (bitFlags ofCapabilities (ofp_capabilities f))
  putWord32be $ ofp_capabilities f
--  putWord32be (bitFlags ofActionType   (ofp_actions f))
  putWord32be $ ofp_actions f
  mapM_ putOfpPort (ofp_ports f)

putMessageData (OfpEchoReply bs) = putByteString bs

putMessageData (OfpGetConfigReply cfg) = do
  putWord16be (fromIntegral (fromEnum (ofp_switch_cfg_flags cfg)))
  putWord16be (ofp_switch_cfg_miss_send_len cfg)

putMessageData (OfpErrorReply et) = do
  putWord16be (fromIntegral $ ofErrorType (ofp_error_type et))
  putWord16be (fromIntegral $ ofErrorCode (ofp_error_type et))
  putByteString (BS.take 64 (ofp_error_data et))

putMessageData OfpEmptyReply = return ()

putMessageData (OfpPacketInReply p) = putOfpPacketIn p

-- FIXME: typed error handling
putMessageData _        = error "Unsupported message: "

-- FIXME: change to something more effective
bitFlags :: Num b => (a -> b) -> S.Set a -> b
bitFlags fn fs = S.fold (\v acc -> acc + (fn v)) 0 fs

putOfpPort :: OfpPhyPort -> PutM ()
putOfpPort port = do
  putWord16be (ofp_port_no port)                                                            -- 2
  mapM_ putWord8 (drop 2 (unpack64 (ofp_port_hw_addr port)))                                -- 8
  putASCIIZ 16 (ofp_port_name port)    -- ASCIIZ(16)
  putWord32be $ ofp_port_config port      --(bitFlags ofConfigFlags (ofp_port_config port))
  putWord32be $ ofp_port_state port       --(bitFlags ofStateFlags (ofp_port_state port))
  putWord32be $ ofp_port_current port     --(bitFlags ofFeatureFlags (ofp_port_current port))
  putWord32be $ ofp_port_advertised port  --(bitFlags ofFeatureFlags (ofp_port_advertised port))
  putWord32be $ ofp_port_supported port   --(bitFlags ofFeatureFlags (ofp_port_supported port))
  putWord32be $ ofp_port_peer port        --(bitFlags ofFeatureFlags (ofp_port_peer port))

putOfpPacketIn :: OfpPacketIn -> PutM ()
putOfpPacketIn pktIn = do
  putWord32be (ofp_pkt_in_buffer_id pktIn)
  putWord16be (fromIntegral $ BS.length (ofp_pkt_in_data pktIn))
  putWord16be (ofp_pkt_in_in_port pktIn)
  putWord8    (fromIntegral $ fromEnum (ofp_pkt_in_reason pktIn))
  putWord8    0 -- padding
  putByteString (ofp_pkt_in_data pktIn)

