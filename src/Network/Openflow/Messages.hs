module Network.Openflow.Messages ( ofpHelloRequest
                                 , ofpParsePacket
                                 , bsStrict
                                 , putMessage
                                 ) where

import Network.Openflow.Types
import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Bits
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Control.Monad

bsStrict = BS.concat . BL.toChunks

bsLazy s = BL.fromChunks [s]

ofpHeaderLen = (8 + 8 + 16 + 32) `div` 8

ofpHelloRequest :: Word8 -> Word32 -> PutM ()
ofpHelloRequest v xid = putMessageHeader 0 h
  where h = OfpHeader { ofp_hdr_version = v
                      , ofp_hdr_type    = OFPT_HELLO
                      , ofp_hdr_length  = fromIntegral ofpHeaderLen
                      , ofp_hdr_xid     = xid
                      }


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
  putWord32be (ofp_n_tables f)
  putWord32be (bitFlags ofCapabilities (ofp_capabilities f))
  putWord32be 0 -- reserved, see OpenFlow Spec. 1.1
  mapM_ putOfpPort (ofp_ports f)

putMessageData _        = error "Unsupported message: "

bitFlags :: Num b => (a -> b) -> S.Set a -> b
bitFlags fn fs = S.fold (\v acc -> acc + (fn v)) 0 fs

putOfpPort :: OfpPhyPort -> PutM ()
putOfpPort port = do
  putWord16be (ofp_port_no port)
  mapM_ putWord8 (drop 2 (unpack64 (ofp_port_hw_addr port)))
  putByteString (BS.take 15 (ofp_port_name port)) >> putWord8 0 -- ASCIIZ(15) string
  putWord32be (bitFlags ofConfigFlags (ofp_port_config port))
  putWord32be (bitFlags ofStateFlags (ofp_port_state port))
  putWord32be (bitFlags ofFeatureFlags (ofp_port_current port))
  putWord32be (bitFlags ofFeatureFlags (ofp_port_advertised port))
  putWord32be (bitFlags ofFeatureFlags (ofp_port_supported port))
  putWord32be (bitFlags ofFeatureFlags (ofp_port_peer port))

unpack64 :: Word64 -> [Word8]
unpack64 x = map (fromIntegral.(shiftR x)) [56,48..0]

