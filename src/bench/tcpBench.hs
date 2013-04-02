{-# Language OverloadedStrings #-}
module Main where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import Criterion.Main
import HCProbe.TCP
import Data.Word
import qualified Data.ByteString as BS

pktNum :: Word32
pktNum = 1

testTCP :: IPv4Addr
        -> IPv4Addr 
        -> TestPacketTCP
testTCP ip0 ip1 = do
  TestPacketTCP { dstMAC = 0 
                , srcMAC = 1 
                , srcIP  = ip0 
                , dstIP  = ip1 
                , dstPort = 32564 
                , srcPort = 23523
                , testWSS = Just 16384 
                , testFlags = ack 
                , testPayloadLen = 32 
                , testPayload = BS.replicate 32 0
                , testSeqNo = Nothing
                , testAckNo = Nothing
                , testIpID = Nothing
                }

ack :: Word8
ack = (fromIntegral . fromEnum) ACK

main :: IO ()
main = defaultMain 
  [ bench "test/tcp" $ nf makeEthernetFrame  (testTCP 50 150)]

