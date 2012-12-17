{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Messages
import Network.Openflow.Misc
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.Generator
import HCProbe.FakeSwitch

import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Text.Printf
import Data.Maybe

import System.Random
import System.Environment (getArgs)

data ARPExample = ARPExample

instance EthernetFrame ARPExample where
  dstMacAddress  _ = 0xFFFFFFFFFFFF
  srcMacAddress  _ = 0x080046A76E35
  vlanID         _ = Just 253
  typeCode       _ = 0x806 
  putPayload     _ = mapM_ putWord8 arpRaw 

arpRaw :: [Word8]  
arpRaw = [ 0x00, 0x01, 0x08, 0x00, 0x06, 0x04, 0x00, 0x01, 
           0x08, 0x00, 0x46, 0xA7, 0x6E, 0x35, 0x0A, 0x0A,
           0x0A ,0x17, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
           0x0A, 0x0A, 0x0A, 0x16 
         ]


main = do
  let bs = bsStrict $ runPut $ putEthernetFrame ARPExample
  putStr (hexdumpBs 16 " " "\n" bs)
