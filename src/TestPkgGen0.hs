{-# Language OverloadedStrings, MagicHash #-}
module Main where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Misc

import qualified Data.ByteString as BS

import Data.Int
import Control.Monad
import System.IO
import Data.Word
import Data.Bits

import Nettle.OpenFlow.StrictPut

import Network.Openflow.Misc (unpack64)

data TestEthernetFrame = TestEthernetFrame !Int !MACAddr !MACAddr

instance EthernetFrame TestEthernetFrame where
  dstMacAddress  (TestEthernetFrame _ a _) = a
  {-# INLINE dstMacAddress #-}
  srcMacAddress (TestEthernetFrame _ _ b)  = b 
  {-# INLINE srcMacAddress #-}
  vlanID         = const Nothing
  {-# INLINE vlanID #-}
  typeCode       = const 0x806
  {-# INLINE typeCode #-}
  putPayload  (TestEthernetFrame n _ _)  = putEmptyPayload n 
  {-# INLINE putPayload #-}

makeEthernetFrameStrict :: EthernetFrame a => a -> BS.ByteString
makeEthernetFrameStrict x = runPutToByteString 2048 frame
  where
    frame    = do
      putMAC (dstMacAddress x)
      putMAC (srcMacAddress x)
      putVLAN (vlanID x) 
      putWord16be (typeCode x)
      putPayload x 
      putWord32be 0

pktNum = 1000000

main = do
  let pl = BS.replicate  1024 0
  forM_ [1..pktNum] $ \i -> do
    let s = makeEthernetFrameStrict (TestEthernetFrame 64 i i) 
    BS.hPutStr stdout s
