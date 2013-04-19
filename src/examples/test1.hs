{-# LANGUAGE OverloadedStrings #-}
-- | This module creates simple default switch without user program
-- that can be used for testing purposes:
--
-- This program should be as simple as possibe
module Main
  where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_, forM_)
import Control.Monad.Trans (lift)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Data.Bits                -- for IP creation [ TODO: remove ]
import HCProbe.EDSL
-- low level message generation
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Ethernet.IPv4
import Network.Openflow.Ethernet.TCP
import HCProbe.Ethernet
import HCProbe.TCP

main :: IO ()
main = do 
    let ip = 15 .|. (0x10 `shiftL` 24) -- TODO: make ip reasonable
    (s1, s2) <- config $ do
                s1 <- switch ip $ do
                          features $ do -- create 2 ports
                            addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                            addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                s2 <- switchOn s1 (return ())
                return (s1,s2)
    
    lock <- newEmptyMVar
    async $ withSwitch s2 "localhost" 6633 $ do
                -- wait for type examples: 
                lift $ putStr "waiting for barrier request.. "
                waitForType OFPT_BARRIER_REQUEST

                let port = 0
                    m1   = 37 -- TODO gen mac here
                    m2   = 29 -- TODO gen mac here
                let pl = putEthernetFrame . (EthFrameP m1 m2) . putIPv4Pkt $
                            TestPacketTCP { dstMAC = m2
                                          , srcMAC = m1
                                          , srcIP  = 99
                                          , dstIP  = 66
                                          , dstPort = 22
                                          , srcPort = 12342
                                          , testWSS = Just 3
                                          , testFlags = tcpFlagsOf [ACK]
                                          , testPayloadLen = 32
                                          , testAckNo = Nothing
                                          , testSeqNo = Nothing
                                          , testIpID = Nothing
                                          }
                -- correct message
                replicateM_ 10 $ do
                        lift $ putStr "sending.. "
                        bid <- sendOFPPacketIn port 43 pl
                        waitForBID bid
                        lift $ putStrLn "done"

                -- broken length
                let msg = putOFMessage $ do
                            putOFHeader $ do
                              putHdrType OFPT_PACKET_IN
                              putHdrLength 42
                            putPacketIn $ do
                              putPacketInData pl
                send msg
                lift $ putMVar lock ()

                -- correct message
                replicateM_ 10 $ do
                        bid <- sendOFPPacketIn port 43 pl
                        waitForBID bid
    withSwitch s1 "localhost" 6633 $ do
       _ <- lift $ takeMVar lock
       let port = 0
           m1   = 37 -- TODO gen mac here
           m2   = 29 -- TODO gen mac here
       let pl = putEthernetFrame . (EthFrameP m1 m2) . putIPv4Pkt $
                    TestPacketTCP { dstMAC = m2
                                  , srcMAC = m1
                                  , srcIP  = 99
                                  , dstIP  = 66
                                  , dstPort = 22
                                  , srcPort = 12342
                                  , testWSS = Just 3
                                  , testFlags = tcpFlagsOf [ACK]
                                  , testPayloadLen = 32
                                  , testAckNo = Nothing
                                  , testSeqNo = Nothing
                                  , testIpID = Nothing
                                  }
       bid <- sendOFPPacketIn port 43 pl
       waitForBID bid
       lift $ putStrLn "ok"


