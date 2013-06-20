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
import Text.Printf

main :: IO ()
main = do 
    fakeSw <- config $ do
                switch $ do
                    addMACs [1..450]
                    features $ do
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def

    lSE <- sequence $ map (\_->initPacketStats 1000 0.5) [1..100]

    print fakeSw
    withSwitch fakeSw "127.0.0.1" 6633 $ do
       
        let stEnt = head lSE
        setStatsHandler stEnt $ \StatEntry{statBid=bid,statRoundtripTime=rtt} ->
            putStr $ printf "bid: %6d rtt: %4.2fms\n" bid (realToFrac rtt * 1000 :: Double)

{-
        xid <- nextXID
        statsSend stEnt $ putOFMessage $ do
                             putOFHeader $ do
                               putHdrVersion openflow_1_0
                               putHdrType OFPT_HELLO
                               putHdrXid xid
-}                            

        -- wait for type examples: 
        lift $ putStr "waiting for barrier request.. "
        waitForType OFPT_BARRIER_REQUEST
        lift $ putStrLn  "[done]"
        lift $ putStr "waiting for echo request.. "
        --waitForType OFPT_ECHO_REQUEST
        lift $ putStrLn "[done]"
        
        -- thread delay example
        lift $ putStr "waiting for 1 second.. "
        lift $ threadDelay 1000000 -- wait for a second
        lift $ putStrLn "[done]"
        
        -- next buffer id example
        replicateM_ 10 $ do
            x <- nextBID
            lift . putStrLn $ "next buffer id " ++ show x

        count <- lift $ ( newIORef 0 :: IO (IORef Int))

        -- Sending primitives:
        -- send simple packet
        lift $ putStrLn "sending packet. and waiting for responce."
        let port = 1
            m1   = 37
            m2   = 29
        dstGenMac <- genLocalMAC
        srcGenMac <- genLocalMAC
        let pl = putEthernetFrame . (EthFrame m1 m2) . putIPv4Pkt $
                    TestPacketTCP { dstMAC = dstGenMac
                                  , srcMAC = srcGenMac
                                  , srcIP  = 99
                                  , dstIP  = 66
                                  , dstPort = 22
                                  , srcPort = 12342
                                  , testWSS = Just 3
                                  , testFlags = tcpFlagsOf [ACK]
                                  , testPayloadLen = 64000
                                  , testAckNo = Nothing
                                  , testSeqNo = Nothing
                                  , testIpID = Nothing
                                  }
        bid <- statsSendOFPPacketIn stEnt port pl
        waitForBID bid
        lift $ threadDelay 1000000

        -- send packet with broken length
        let msg = putOFMessage $ do
                      putOFHeader $ do
                          putHdrType OFPT_PACKET_IN
                          putPacketLength 9109
                      putPacketIn $ do
                          putPacketInData pl
        lift $ print msg
        send msg


        lift $ putStrLn "done"
    
    stats <- assembleStats lSE 
    print stats
