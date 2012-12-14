module Network.Openflow.Misc ( unpack64, putMAC, putASCIIZ ) where

import Network.Openflow.Types
import Data.Word
import Data.Bits
import Data.Binary.Put
import qualified Data.ByteString as BS
import Control.Monad

unpack64 :: Word64 -> [Word8]
unpack64 x = map (fromIntegral.(shiftR x)) [56,48..0]

putASCIIZ :: Int -> BS.ByteString -> PutM ()
putASCIIZ sz bs = putByteString bs' >> replicateM_ (sz - (BS.length bs')) (putWord8 0)
  where bs' = BS.take (sz - 1) bs

putMAC :: MACAddr -> PutM ()
putMAC mac = mapM_ putWord8 (take 2 (unpack64 mac))

