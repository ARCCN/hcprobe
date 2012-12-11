module Network.Openflow.Messages ( ofpHelloRequest
                                 , ofpParseHeader
                                 , bsStrict
                                 ) where

import Network.Openflow.Types
import Data.Binary.Put
import Data.Binary.Get
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
                      , ofp_hdr_type    = OFPT_ECHO_REQUEST
                      , ofp_hdr_length  = fromIntegral ofpHeaderLen
                      , ofp_hdr_xid     = xid
                      }


-- FIXME: error handling
ofpParseHeader :: BS.ByteString -> Maybe OfpHeader
ofpParseHeader s = flip runGet (bsLazy s) $ do
    v   <- getWord8
    tp  <- getWord8
    len <- getWord16be
    xid <- getWord32be
    return $ Just (OfpHeader v (toEnum (fromIntegral tp)) len xid)

