{-# Language BangPatterns #-}
module Network.Openflow.Misc ( unpack64, putMAC, putIP, putASCIIZ,
                               putWord16le,
                               bsStrict, bsLazy, encodePutM, ipv4,
                               csum16, csum16', csum16'', hexdumpBs, icsum16', fin_icsum16'
                             ) where

import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Data.Word
import Data.Bits
import qualified Data.Vector.Storable as V
import Control.Applicative
import Network.Openflow.StrictPut
import qualified Data.Binary.Put as BP 
import Data.Binary.Get
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import Control.Monad

import Foreign.Storable
import Foreign.ForeignPtr

import Text.Printf
import Debug.Trace

-- TODO: move to Nettle.OpenFlow.StrictPut
putWord16le :: Word16 -> PutM ()
putWord16le w = trace (printf "CS16: %02x\n" w) $ putWord8 (fromIntegral $ w .&. 0xFF) >> putWord8 (fromIntegral w `shiftR` 8)
{-# INLINE putWord16le #-}

-- TODO: may be improved by unsafe shifts
unpack64 :: Word64 -> [Word8]
unpack64 x = map (fromIntegral.(shiftR x)) [56,48..0]
{-# INLINE unpack64 #-}

putASCIIZ :: Int -> BS.ByteString -> PutM ()
putASCIIZ sz bs = putByteString bs' >> replicateM_ (sz - (BS.length bs')) (putWord8 0)
  where bs' = BS.take (sz - 1) bs

putMAC :: MACAddr -> PutM ()
putMAC mac = do
  putWord16be (fromIntegral $ mac' `shiftR` 32)
  putWord32be (fromIntegral $ mac .&. 0xFFFFFFFF)
  where mac' = (mac .&. 0xFFFFFFFFFFFF)
{-# INLINE putMAC #-}

putIP :: IPv4Addr -> PutM ()
putIP ip = putWord32be ip
{-# INLINE putIP #-}

bsStrict :: BL.ByteString -> BSI.ByteString
bsStrict = BS.concat . BL.toChunks

bsLazy :: BSI.ByteString -> BL.ByteString
bsLazy s = BL.fromChunks [s]

hexdumpBs :: Int -> String -> String -> BS.ByteString -> String
hexdumpBs n ds ts bs = concat $ concat rows
  where hexes :: [String]
        hexes = reverse $ BS.foldl (\acc w -> (printf "%02X" w :: String) : acc) [] bs
        rows  = unfoldr chunk hexes
        chunk [] = Nothing
        chunk xs = Just (intersperse ds (take n xs) ++ [ts], drop n xs)

encodePutM :: BP.Put -> BSI.ByteString
encodePutM = bsStrict . BP.runPut

ipv4 :: Word8->Word8->Word8->Word8 -> IPv4Addr
ipv4 a b c d = wa .|. wb .|. wc .|. wd
  where wa = fromIntegral a `shiftL` 24
        wb = fromIntegral b `shiftL` 16
        wc = fromIntegral c `shiftL`  8
        wd = fromIntegral d

csum16 :: BS.ByteString -> Word16
csum16 bs = withResult (pushEndOfInput $ pushChunk (runGetIncremental crc) bs)
  where
    withResult (Done _ _ x) = x
    withResult _ = 0 
    crc = rotate' . trunc <$> go (0::Word32)
    go !x = do
      e <- isEmpty
      if e then return x
           else do y <- fromIntegral <$> getWord16le
                   go (x+y)
    {-# INLINE go #-}
{-# INLINE csum16 #-}

trunc :: Word32 -> Word16
trunc w = fromIntegral $ complement $ (w .&. 0xFFFF) + (w `shiftR` 16)
{-# INLINE trunc #-}

rotate' :: Word16 -> Word16
rotate' x = x `rotateL` 8
{-# INLINE rotate' #-}

csum16' :: BS.ByteString -> Word16
csum16' = fin_icsum16' . (icsum16' 0)
{-# INLINE csum16' #-}

icsum16' :: Word32 -> BS.ByteString -> Word32
icsum16' i bs = V.foldl' (\a -> (+a).fromIntegral) (i :: Word32) bv
  where bv = byteStringToVector bs :: V.Vector Word16
{-# INLINE icsum16' #-}

fin_icsum16' :: Word32 -> Word16
fin_icsum16' = rotate' . trunc
{-# INLINE fin_icsum16' #-}

csum16'' :: BS.ByteString -> Word16
csum16'' bs = r $ csum bs
  where csum = BS.foldl' fn (0 :: Word32, False, 0 :: Word16)
        fn (crc, False, _) v  = (crc, True, fromIntegral v)
        fn (crc, True, x)  v  = (crc + fromIntegral (x + ((fromIntegral v) `shiftR` 8)), False, 0)
        {-# INLINE fn #-}
        r  (crc, _, _) = (trunc) crc
{-# INLINE csum16'' #-}

byteStringToVector :: (Storable a) => BS.ByteString -> V.Vector a
byteStringToVector bs = vec 
  where
    vec = V.unsafeFromForeignPtr (castForeignPtr fptr) (scale off) (scale len)
    (fptr, off, len) = BSI.toForeignPtr bs
    scale = (`div` sizeOfElem vec)
    sizeOfElem :: (Storable a) => V.Vector a -> Int
    sizeOfElem vec = sizeOf (undefined `asTypeOf` V.head vec)
{-# INLINE byteStringToVector  #-}
