{-# LANGUAGE BangPatterns #-}
module Network.Openflow.Ethernet.IPv4 (IPv4Flag(..), IPv4(..), putIPv4Pkt) where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Data.Word
import qualified Data.ByteString as BS
import Network.Openflow.StrictPut
import Data.Bits

import Data.Maybe
import Text.Printf
import Debug.Trace

data IPv4Flag = DF | MF | Res deriving (Eq, Ord, Show, Read)

instance Enum IPv4Flag where
  fromEnum DF   = 4
  fromEnum MF   = 2
  fromEnum Res  = 1

class IPv4 a where
  ipHeaderLen  :: a -> Word8
  ipVersion    :: a -> Word8
  ipTOS        :: a -> Word8
  ipID         :: a -> Word16
  ipFlags      :: a -> Word8
  ipFragOffset :: a -> Word16
  ipTTL        :: a -> Word8 
  ipProto      :: a -> Word8
  ipSrc        :: a -> IPv4Addr
  ipDst        :: a -> IPv4Addr
  ipPutPayload :: a -> PutM ()

putIPv4Pkt :: IPv4 a => a -> PutM ()
putIPv4Pkt x = do
  let !hdrF = hdr
  let !crc16 = csum16' hdrF
  putHdr (Just crc16) >> putByteString body
  where
    hdr  = runPutToByteString  32   (putHdr Nothing)
    body = runPutToByteString 1800  (ipPutPayload x)

    putHdr Nothing = do
      putWord8    lenIhl     -- version, ihl
      putWord8    tos
      putWord16be totLen
      putWord16be ipId
      putWord16be flagsOff
      putWord8    ttl
      putWord8    proto
      putWord16be 0 -- crc
      putIP       ipS
      putIP       ipD

    putHdr (Just cs) = do
      putByteString (BS.take 10 hdr)
      putWord16be cs -- crc
      putByteString (BS.drop 12 hdr)

    lenIhl = (ihl .&. 0xF) .|. (ver `shiftL` 4 .&. 0xF0)
    ihl    = ipHeaderLen x
    ver    = ipVersion x
    tos    = ipTOS x
    ipId   = ipID x
    flagsOff = (off .&. 0x1FFF) .|. ((fromIntegral flags) `shiftL` 13)
    flags  = ipFlags x
    totLen = fromIntegral $ 4*(ipHeaderLen x) + fromIntegral (BS.length body)
    off    = ipFragOffset x
    ttl    = ipTTL x
    proto  = ipProto x
    ipS    = ipSrc x
    ipD    = ipDst x

