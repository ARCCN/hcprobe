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
    fakeSw <- config $ do
                switch $ do
                    addMACs [1..450]
                    features $ do
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
    print fakeSw
    
    lSE <- sequence $ map (\_->initPacketStats 1000 0.5) [1..100]

    withSwitch fakeSw "127.0.0.1" 6633 $ do
       
	lift $ putStrLn "start"

        let stEnt = head lSE
        setSilentStatsHandler stEnt
        
        -- thread delay
        lift $ putStr "waiting for 1 second.. "
        lift $ threadDelay 1000000 -- wait for a second
        lift $ putStrLn "[done]"

        -- payload for PacketIn
        lift $ putStrLn "sending packet. and waiting for responce."
        let port = 1
        dstGenMac <- genLocalMAC
        srcGenMac <- genLocalMAC
        let pl = putEthernetFrame . (EthFrame dstGenMac srcGenMac) . putIPv4Pkt $
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
                      putPortStatusPortDirect (head . ofp_switch_features_ports . eSwitchFeatures $ fakeSw)
        send msg
        lift $ threadDelay 10000
        
        -- error
        x <- nextBID
        lift . putStrLn $ "next buffer id " ++ show x
        let msg = putOFMessage $ do
                    putOFHeader $ do
                      putHdrType OFPT_ERROR
                    putErrorMessage $ do
                      putErrorType (OFPET_BAD_ACTION OFPBAC_BAD_TYPE)
                      putErrorData $ BS.pack $ replicate 64 42
        send msg
        lift $ threadDelay 10000

        -- barrier
        x <- nextBID
        lift . putStrLn $ "next buffer id " ++ show x
        let msg = putOFMessage $ do
                    putOFHeader $ do
                      putHdrType OFPT_BARRIER_REPLY
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
        
        -- features reply
        {-
        x <- nextBID
        lift . putStrLn $ "next buffer id " ++ show x
        let msg = putOFMessage $ do
                    putOFHeader $ do
                      putHdrType OFPT_FEATURES_REPLY
                      putHdrXid 241241
                    putFeaturesReply $ do
                      putSwitchFeaturesDatapathId   1
                      putSwitchFeaturesNBuffers     10000
                      putSwitchFeaturesNTables      1
                      putSwitchFeaturesCapabilities (ofp_switch_features_capabilities . eSwitchFeatures $ fakeSw)
                      putSwitchFeaturesActions      (ofp_switch_features_actions . eSwitchFeatures $ fakeSw)
                      putSwitchFeaturesPorts        (ofp_switch_features_ports . eSwitchFeatures $ fakeSw)
        send msg
        lift $ threadDelay 10000
        
        -- get config reply
        x <- nextBID
        cfg <- currentSwitchConfig
        lift . putStrLn $ "next buffer id " ++ show x
        let msg = putOFMessage $ do
                    putOFHeader $ do
                      putHdrType OFPT_GET_CONFIG_REPLY
                      putHdrXid 2414242
                    putGetConfigReply $ do
                      putSwitchCfgFlags   (ofp_switch_cfg_flags cfg)
                      putSwitchCfgMissSendLen     128
        send msg
        lift $ threadDelay 10000
	-}
	lift $ threadDelay 1000000

	lift $ putStrLn "done"
