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
import Data.Maybe
import Control.Monad
import System.IO
import System.Environment
import qualified Data.ByteString as BS

import Safe

pktNum = 1

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
  pnum <- liftM (maybe pktNum read.headMay) getArgs
  let pkts = [testTCP i (i*3)  | i <- [1..pnum]]
  let chunks = map makeEthernetFrame pkts
  mapM_ (BS.hPutStr stdout) chunks 

