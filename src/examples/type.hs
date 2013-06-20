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
import HCProbe.EDSL
-- low level message generation
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Ethernet.IPv4
import Network.Openflow.Ethernet.TCP
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

    withSwitch fakeSw "127.0.0.1" 6633 $ do
       
	lift $ putStrLn "start"
        
        -- thread delay
        lift $ putStr "waiting for 1 second.. "
        lift $ threadDelay 1000000 -- wait for a second
        lift $ putStrLn "[done]"

        -- send simple packet
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

	-- broken type (Packet In => Port Status)
	replicateM_ 100 $ do
            let msg = putOFMessage $ do
                  putOFHeader $ do
                    putHdrType OFPT_PACKET_IN
                  putPortStatus $ do
                    putPortStatusReason OFPR_ADD
                    putPortStatusPortDirect (head.ofp_switch_features_ports.eSwitchFeatures $ fakeSw)
            send msg
            lift $ threadDelay 10000
	
	lift $ threadDelay 1000000

	lift $ putStrLn "done"
