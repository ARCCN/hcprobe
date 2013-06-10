{-# LANGUAGE OverloadedStrings #-}
-- | This module creates simple default switch without user program
-- that can be used for testing purposes:
--
-- This program should be as simple as possibe
module Main
  where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
--import Control.Monad (replicateM_)
import Control.Monad.Trans (lift)
import Data.Bits                -- for IP creation [ TODO: remove ]
import HCProbe.EDSL
import Control.Monad
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
    fakeSwList <- forM [1..2] $ \i -> do
          config $ do
                switch $ do
                    addMACs [(i*400)+1..(i+1)*400]
                    features $ do
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def

    lSE <- sequence $ map (\_->initPacketStats 1000 0.5) [1..2]

    let swsWithSes = zip fakeSwList lSE
    
    let putPl dstMac srcMac = putEthernetFrame . (EthFrame dstMac srcMac) . putIPv4Pkt $
                TestPacketTCP { dstMAC = dstMac
                              , srcMAC = srcMac
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
    
    let (fs, stE) = head swsWithSes
    a1 <- async $ withSwitch fs "127.0.0.1" 6633 $ do
       
        let stEnt = stE
        setStatsHandler stEnt $ \StatEntry{statBid=bid,statRoundtripTime=rtt} ->
            putStr $ printf "bid: %6d rtt: %4.2fms\n" bid (realToFrac rtt * 1000 :: Double)
        
        -- thread delay example
        lift $ threadDelay 1000000 -- wait for a second

        replicateM_ 100 $ do
            let port = 1
            dstGenMac <- genLocalMAC
            srcGenMac <- genLocalMAC
            bid <- statsSendOFPPacketIn stEnt port (putPl dstGenMac srcGenMac)
            return()         

        lift $ threadDelay 3000000
        lift $ putStrLn "done"
        
    let (fs, stE) = last swsWithSes    
    a2 <- async $ withSwitch fs "127.0.0.1" 6633 $ do
    
        let stEnt = stE
        setStatsHandler stEnt $ \StatEntry{statBid=bid,statRoundtripTime=rtt} ->
            putStr $ printf "bid: %6d rtt: %4.2fms\n" bid (realToFrac rtt * 1000 :: Double)
        
        -- thread delay example
        lift $ threadDelay 1000000 -- wait for a second

        replicateM_ 100 $ do
            x <- nextBID
            dstGenMac <- genLocalMAC
            srcGenMac <- genLocalMAC
            let msg = putOFMessage $ do
                  putOFHeader $ do
                    putHdrType OFPT_PACKET_IN
                    putPacketLength 42
                  putPacketIn $ do
                    putPacketInBufferId x 
                    putPacketInData $ putPl dstGenMac srcGenMac
            send msg      

        lift $ threadDelay 3000000
        lift $ putStrLn "done"
        
    mapM_ waitCatch [a1,a2]
    
    stats <- assembleStats lSE 
    print stats
