{-# Language BangPatterns #-}
module Network.Openflow.Ethernet.Generator where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Network.Openflow.StrictPut
import Data.Digest.CRC32
import Data.Word
import Data.Monoid
import qualified Data.ByteString as BS
import Control.Monad
import Data.ByteString.Lazy.Builder

import Debug.Trace

-- TODO: possibly, there is a way to calculate crc32 on the fly,
--       without generating the actual bytestring         

putEthernetFrame :: EthernetFrame a => a -> PutM ()
putEthernetFrame x = putFrame
  where
    putFrame = do
      putMAC (dstMacAddress x)
      putMAC (srcMacAddress x)
      putVLAN (vlanID x) 
      putWord16be (typeCode x)
      putPayload x
      putWord32be 0 -- FIXME: CRC!
    {-# INLINE putFrame #-}

    -- FIXME: fix checksum calculation
    putCRC32 = putWord32be 0 -- (crc32 bs)
    {-# INLINE putCRC32 #-}
{-# INLINABLE putEthernetFrame #-}

buildEthernetFrame :: EthernetFrame a => a -> Builder
buildEthernetFrame x =
       buildMAC   (dstMacAddress x)
    <> buildMAC   (srcMacAddress x)
    <> buildVLAN  (vlanID x)
    <> word16BE   (typeCode x)
    <> byteString pl
    <> word32BE   0
    where pl = runPutToByteString 32768 (putPayload x) -- FIXME use only Builder

makeEthernetFrame :: EthernetFrame a => a -> BS.ByteString
makeEthernetFrame = runPutToByteString 2048 . putEthernetFrame
{-# INLINABLE  makeEthernetFrame #-}
{-# DEPRECATED makeEthernetFrame "use putEthernetFrame instead" #-}

putVLAN :: Maybe Word16 -> PutM ()
putVLAN Nothing     = return () 
putVLAN (Just vlan) = putWord16be 0x8100 >> putWord16be vlan
{-# INLINABLE putVLAN #-}

buildVLAN :: Maybe Word16 -> Builder
buildVLAN Nothing = mempty
buildVLAN (Just vlan) = word16BE 0x8100 <> word16BE vlan
{-# INLINABLE buildVLAN #-}

putEmptyPayload :: Int -> PutM ()
putEmptyPayload n | n `mod` 8 == 0 = replicateM_ (n `div` 8) (putWord64be 0)
                  | n `mod` 4 == 0 = replicateM_ (n `div` 4) (putWord32be 0)
                  | n `mod` 2 == 0 = replicateM_ (n `div` 2) (putWord16be 0)
                  | otherwise      = replicateM_ n           (putWord8    0)
{-# INLINABLE putEmptyPayload #-}

