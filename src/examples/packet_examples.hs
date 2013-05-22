{-# LANGUAGE OverloadedStrings #-}
-- | This module creates simple default switch without user program
-- that can be used for testing purposes:
--
-- This program should be as simple as possibe
module Main
  where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Control.Monad.Trans (lift)
import Data.Bits                -- for IP creation [ TODO: remove ]
import qualified Data.ByteString as BS
import Data.Word

import HCProbe.EDSL
-- low level message generation
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Ethernet.IPv4
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Types
import HCProbe.Ethernet
import HCProbe.TCP
import HCProbe.EDSL.Handlers
import Data.IORef

main :: IO ()
main = do 
    let ip = 15 .|. (0x10 `shiftL` 24) -- TODO: make ip reasonable
    fakeSw <- config $ do
                switch ip $ do
                    addMACs [1..450]
                    features $ do
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
    print fakeSw

    lSE <- sequence $ map (\_->initPacketStats 1000 0.5) [1..100]

    withSwitch fakeSw "127.0.0.1" 6633 $ do
       
	lift $ putStrLn "start"

        let stEnt = head lSE
        setStatsHandler stEnt
        
        -- thread delay
        lift $ putStr "waiting for 1 second.. "
        lift $ threadDelay 1000000 -- wait for a second
        lift $ putStrLn "[done]"

        -- payload for PacketIn
        lift $ putStrLn "sending packet. and waiting for responce."
        let port = 1
        dstGenMac <- genLocalMAC
        srcGenMac <- genLocalMAC
        let pl = putEthernetFrame . (EthFrameP dstGenMac srcGenMac) . putIPv4Pkt $
                    TestPacketTCP { dstMAC = dstGenMac
                                  , srcMAC = srcGenMac
                                  , srcIP  = 99
                                  , dstIP  = 66
                                  , dstPort = 80
                                  , srcPort = 12342
                                  , testWSS = Just 3
                                  , testFlags = tcpFlagsOf [ACK]
                                  , testPayloadLen = 32
                                  , testAckNo = Nothing
                                  , testSeqNo = Nothing
                                  , testIpID = Nothing
                                  }
        --bid <- statsSendOFPPacketIn stEnt port pl
        --waitForBID bid
        --lift $ putStrLn "done"
	
	-- packet in
        x <- nextBID
        lift . putStrLn $ "next buffer id " ++ show x
        let msg = putOFMessage $ do
                    putOFHeader $ do
                      putHdrType OFPT_PACKET_IN
                    putPacketIn $ do
                      putPacketInBufferId x 
                      putPacketInData pl
        send msg
        lift $ threadDelay 10000
        
        -- port status
        x <- nextBID
        lift . putStrLn $ "next buffer id " ++ show x
        let msg = putOFMessage $ do
                    putOFHeader $ do
                      putHdrType OFPT_PORT_STATUS
                    putPortStatus $ do
                      putPortStatusReason OFPR_ADD
                      putPortStatusPortDirect (head.ofp_ports.eSwitchFeatures $ fakeSw)
        send msg
        lift $ threadDelay 10000
        
        -- error
        x <- nextBID
        lift . putStrLn $ "next buffer id " ++ show x
        let msg = putOFMessage $ do
                    putOFHeader $ do
                      putHdrType OFPT_ERROR
                    putErrorMessage $ do
                      putErrorType (OFPET_HELLO_FAILED OFPHFC_INCOMPATIBLE)
                      putErrorData $ BS.pack [42,42]
        send msg
        lift $ threadDelay 10000
        
        -- flow removed
        x <- nextBID
        lift . putStrLn $ "next buffer id " ++ show x
        let msg = putOFMessage $ do
                    putOFHeader $ do
                      putHdrType OFPT_FLOW_REMOVED
                    putFlowRemoved $ do
                      putFlowRemovedCookie       0
                      putFlowRemovedPriority     1
                      putFlowRemovedReason       OFPRR_IDLE_TIMEOUT
                      putFlowRemovedTableId      0
                      putFlowRemovedDurationSec  1000
                      putFlowRemovedDurationNsec 100000
                      putFlowRemovedIdleTimeout  1000
                      putFlowRemovedHardTimeout  1000
                      putFlowRemovedPacketCount  42
                      putFlowRemovedByteCount    241241
        send msg
        lift $ threadDelay 10000
	
	lift $ threadDelay 1000000

	lift $ putStrLn "done"
    
    stats <- assembleStats lSE 
    print stats
