{-# Language OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Int
import Control.Monad
import System.IO
import qualified Data.Binary.Put as P
import Data.Word
import Data.Bits

import qualified Nettle.OpenFlow.StrictPut as SP

import Network.Openflow.Misc (unpack64)

type MACAddress = Word64

putMAC :: MACAddress -> SP.PutM ()
putMAC = mapM_ SP.putWord8.drop 2.unpack64
{-# INLINE putMAC #-}

putTypeCode :: Maybe Word16 -> Word16 -> SP.PutM ()
putTypeCode (Just vlan) tc  = SP.putWord16be 0x8100 >> SP.putWord16be vlan >> SP.putWord16be tc
putTypeCode Nothing     tc  = SP.putWord16be tc
{-# INLINE putTypeCode #-}

putEmptyPayload :: Int -> SP.PutM ()
putEmptyPayload n | n `mod` 8 == 0 = replicateM_ (n `div` 8) (SP.putWord64be 0)
                  | n `mod` 4 == 0 = replicateM_ (n `div` 4) (SP.putWord32be 0)
                  | n `mod` 2 == 0 = replicateM_ (n `div` 2) (SP.putWord16be 0)
                  | otherwise      = replicateM_ n           (SP.putWord8    0)
{-# INLINE putEmptyPayload #-}

makeEthernetFrameStrict :: MACAddress -> MACAddress -> Word16 -> Int -> BS.ByteString
makeEthernetFrameStrict mac1 mac2 tc n = SP.runPutToByteString 2048 frame
  where
    frame    = do
      putMAC mac1
      putMAC mac2
      putTypeCode Nothing tc
      putEmptyPayload n
      SP.putWord32be 0

pktNum = 1000000

main = do
  let pl = BS.replicate  1024 0
  forM_ [1..pktNum] $ \i -> do
    let s = makeEthernetFrameStrict 0 1 0x806 1024
    BS.hPutStr stdout s

