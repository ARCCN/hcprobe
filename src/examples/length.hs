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

        -- send simple packet
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
        lift $ putStrLn "done"

	-- broken length
	replicateM_ 100 $ do
            x <- nextBID
            lift . putStrLn $ "next buffer id " ++ show x
            let msg = putOFMessage $ do
                  putOFHeader $ do
                    putHdrType OFPT_PACKET_IN
                    putHdrLength 0
                  putPacketIn $ do
                    putPacketInBufferId x 
                    putPacketInData pl
            send msg
            lift $ threadDelay 10000
	
	lift $ threadDelay 1000000

	lift $ putStrLn "done"
    
    stats <- assembleStats lSE 
    print stats
