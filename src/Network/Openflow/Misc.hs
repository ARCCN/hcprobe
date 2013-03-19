module Network.Openflow.Misc ( unpack64, putMAC, putIP, putASCIIZ,
                               putWord16le,
                               bsStrict, bsLazy, encodePutM, ipv4,
                               csum16, hexdumpBs
                             ) where

import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Data.Word
import Data.Bits
import Nettle.OpenFlow.StrictPut 
import qualified Data.Binary.Put as BP 
import Data.Binary.Strict.Get
import Data.List
import Data.Either
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Text.Printf
import Debug.Trace

-- TODO: move to Nettle.OpenFlow.StrictPut
putWord16le :: Word16 -> PutM ()
putWord16le w = putWord8 (fromIntegral w `shiftR` 8) >> putWord8 (fromIntegral w)
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

bsStrict = BS.concat . BL.toChunks

bsLazy s = BL.fromChunks [s]

hexdumpBs :: Int -> String -> String -> BS.ByteString -> String
hexdumpBs n ds ts bs = concat $ concat rows
  where hexes :: [String]
        hexes = reverse $ BS.foldl (\acc w -> (printf "%02X" w :: String) : acc) [] bs
        rows  = unfoldr chunk hexes
        chunk [] = Nothing
        chunk xs = Just (intersperse ds (take n xs) ++ [ts], drop n xs)

encodePutM = bsStrict . BP.runPut

ipv4 :: Word8->Word8->Word8->Word8 -> IPv4Addr
ipv4 a b c d = wa .|. wb .|. wc .|. wd
  where wa = fromIntegral a `shiftL` 24
        wb = fromIntegral b `shiftL` 16
        wc = fromIntegral c `shiftL`  8
        wd = fromIntegral d

csum16 :: BS.ByteString -> Maybe Word16
csum16 s = words >>= return . trunc . (foldl' (\acc w -> acc + fromIntegral w) 0)
  where withResult (Left _, _)  = Nothing
        withResult (Right s, _) = Just s
        words = withResult $ flip runGet s (replicateM (BS.length s `div` 2) $ getWord16le)
        trunc :: Word32 -> Word16
        trunc w = fromIntegral $ complement $ (w .&. 0xFFFF) + (w `shiftR` 16)
{-# INLINE csum16 #-}
