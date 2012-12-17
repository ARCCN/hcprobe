{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Messages
import Network.Openflow.Misc
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.ARP
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

data ARPExample = ARPExample { senderMAC :: MACAddr
                             , senderIP  :: IPv4Addr
                             }


instance ARPReply ARPExample where
  hardwareType    x = 0x01
  ipProtocolType  x = 0x800
  hwAddressLen    x = 6
  ipAddressLen    x = 4
  opcode          x = 0x02
  senderHWAddress = senderMAC
  senderIPAddress = senderIP
  targetHWAddress = senderMAC
  targetIPAddress = senderIP


instance EthernetFrame ARPExample where
  dstMacAddress  _ = 0xFFFFFFFFFFFF
  srcMacAddress  _ = 0x080046A76E35
  vlanID         _ = Just 253
  typeCode       _ = 0x806 
  putPayload       = putARPReply

main = do
  let bs = bsStrict $ runPut $ putEthernetFrame (ARPExample 0 0)
  putStr (hexdumpBs 16 " " "\n" bs)
