{-# Language OverloadedStrings #-}
module Main where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Misc
import HCProbe.TCP
import HCProbe.FakeSwitch (mcPrefix)

import Nettle.OpenFlow.StrictPut
import Data.Word
import Control.Monad
import System.IO
import qualified Data.ByteString as BS


pktNum = 1000000

testTCP ip0 ip1 = do
  TestPacketTCP { dstMAC = 0 
                , srcMAC = 1 
                , srcIP  = ip0 
                , dstIP  = ip1 
                , dstPort = 32564 
                , srcPort = 23523
                , testWSS = Just 16384 
                , testFlags = Just [ACK] 
                , testPayloadLen = 32 
                , testSeqNo = Nothing
                , testAckNo = Nothing
                , testIpID = Nothing
                }

main = do
  let pkts = [testTCP i (i*3)  | i <- [1..pktNum]]
  let chunks = map makeEthernetFrame pkts
  mapM_ (BS.hPutStr stdout) chunks 

