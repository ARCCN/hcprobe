module Network.Openflow.Misc ( unpack64, putMAC, putIP, putASCIIZ, 
                               bsStrict, bsLazy, encodePutM, ipv4,
                               hexdumpBs
                             ) where

import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Data.Word
import Data.Bits
import Data.Binary.Put
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Text.Printf

unpack64 :: Word64 -> [Word8]
unpack64 x = map (fromIntegral.(shiftR x)) [56,48..0]

putASCIIZ :: Int -> BS.ByteString -> PutM ()
putASCIIZ sz bs = putByteString bs' >> replicateM_ (sz - (BS.length bs')) (putWord8 0)
  where bs' = BS.take (sz - 1) bs

putMAC :: MACAddr -> PutM ()
putMAC mac = mapM_ putWord8 (drop 2 (unpack64 mac))

putIP :: IPv4Addr -> PutM ()
putIP ip = putWord32be ip

bsStrict = BS.concat . BL.toChunks

bsLazy s = BL.fromChunks [s]

hexdumpBs :: Int -> String -> String -> BS.ByteString -> String
hexdumpBs n ds ts bs = concat $ concat rows
  where hexes :: [String]
        hexes = reverse $ BS.foldl (\acc w -> (printf "%02X" w :: String) : acc) [] bs
        rows  = unfoldr chunk hexes
        chunk [] = Nothing
        chunk xs = Just (intersperse ds (take n xs) ++ [ts], drop n xs)

encodePutM = bsStrict . runPut


ipv4 :: Word8->Word8->Word8->Word8 -> IPv4Addr
ipv4 a b c d = wa .|. wb .|. wc .|. wd
  where wa = fromIntegral a `shiftL` 24
        wb = fromIntegral b `shiftL` 16
        wc = fromIntegral c `shiftL`  8
        wd = fromIntegral d

