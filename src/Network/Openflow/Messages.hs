module Network.Openflow.Messages ( ofpHelloRequest
                                 , ofpParsePacket
                                 , bsStrict
                                 ) where

import Network.Openflow.Types
import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Word ( Word8, Word32 )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Control.Monad

bsStrict = BS.concat . BL.toChunks

bsLazy s = BL.fromChunks [s]

ofpHeaderLen = (8 + 8 + 16 + 32) `div` 8

ofpHelloRequest :: Word8 -> Word32 -> PutM ()
ofpHelloRequest v xid = putOfpHeader h
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

