{-# Language OverloadedStrings #-}
module Main where


import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Misc
import HCProbe.ARP
import HCProbe.FakeSwitch (mcPrefix)

import Network.Openflow.StrictPut
import Data.Word
import Control.Monad
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Builder 

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property

pktNum = 1000000

testARP :: Word64 -> Word32 -> Result
testARP i j = BS.concat (LBS.toChunks (toLazyByteString (buildEthernetFrame a))) ==? (runPutToByteString 32678 (putEthernetFrame a))
  where a = ARPGratuitousReply i j

main = hspec $ describe "bytestring equal to strict put" $ do
          prop "arp" $ testARP

