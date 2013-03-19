{-# Language BangPatterns #-}
module Network.Openflow.Ethernet.Generator where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Nettle.OpenFlow.StrictPut 
import Data.Digest.CRC32
import Data.Word
import Control.Monad


import Debug.Trace

-- TODO: possibly, there is a way to calculate crc32 on the fly,
--       without generating the actual bytestring         

putEthernetFrame :: EthernetFrame a => a -> PutM ()
putEthernetFrame x = putFrame >> putCRC32
  where
    putFrame = do
      putMAC (dstMacAddress x)
      putMAC (srcMacAddress x)
      putTypeCode (vlanID x) (typeCode x)
      putPayload x
    {-# INLINE putFrame #-}

    -- FIXME: fix checksum calculation
    putCRC32 = putWord32be 0 -- (crc32 bs)
    {-# INLINE putCRC32 #-}

putTypeCode :: Maybe Word16 -> Word16 -> PutM ()
putTypeCode (Just vlan) tc  = putWord16be 0x8100 >> putWord16be vlan >> putWord16be tc
putTypeCode Nothing     tc  = putWord16be tc
{-# INLINE putTypeCode #-}

putEmptyPayload :: Int -> PutM ()
putEmptyPayload n | n `mod` 8 == 0 = replicateM_ (n `div` 8) (putWord64be 0)
                  | n `mod` 4 == 0 = replicateM_ (n `div` 4) (putWord32be 0)
                  | n `mod` 2 == 0 = replicateM_ (n `div` 2) (putWord16be 0)
                  | otherwise      = replicateM_ n           (putWord8    0)
{-# INLINE putEmptyPayload #-}

