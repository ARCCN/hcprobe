{-# Language OverloadedStrings #-}
module Main where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Misc
import HCProbe.ARP
import HCProbe.FakeSwitch (mcPrefix)

import Nettle.OpenFlow.StrictPut
import Data.Word
import Control.Monad
import System.IO
import qualified Data.ByteString as BS

pktNum = 1000000

main = do
  let pkts = [ARPGratuitousReply i (fromIntegral i * 3) | i <- [1..pktNum]]
  let chunks = map makeEthernetFrame pkts
  mapM_ (BS.hPutStr stdout) chunks 

