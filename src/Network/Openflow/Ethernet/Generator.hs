{-# Language BangPatterns #-}
module Network.Openflow.Ethernet.Generator where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Data.Binary.Put
import Data.Digest.CRC32
import Data.Word


import Debug.Trace

-- TODO: possibly, there is a way to calculate crc32 on the fly,
--       without generating the actual bytestring         

putEthernetFrame :: EthernetFrame a => a -> PutM ()
putEthernetFrame x = putByteString frame >> putCRC32 frame
  where
    frame = bsStrict $ runPut $ do
      putMAC (dstMacAddress x)
      putMAC (srcMacAddress x)
      case (vlanID x) of
        Nothing    -> putWord16be (typeCode x)
        Just (vid) -> putWord16be 0x8100 >> putWord16be vid >> putWord16be (typeCode x)
      putPayload x
    putCRC32 bs = putWord32le (crc32 bs)

