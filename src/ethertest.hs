{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Messages
import Network.Openflow.Misc
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.ARP
import Network.Openflow.Ethernet.Generator
import HCProbe.FakeSwitch
import HCProbe.ARP

import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Text.Printf
import Data.Maybe

import System.Random
import System.Environment (getArgs)


main = do
  let bs = bsStrict $ runPut $ putEthernetFrame (ARPGratuitousReply 0 0)
  putStr (hexdumpBs 16 " " "\n" bs)
