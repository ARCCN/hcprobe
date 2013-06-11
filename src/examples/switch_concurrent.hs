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
    fakeSwList <- forM [1..10] $ \i -> do
          config $ do
                switch $ do
                    addMACs [(i*400)+1..(i+1)*400]
                    features $ do
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def

    lSE <- sequence $ map (\_->initPacketStats 1000 0.5) [1..10]

    let swsWithSes = zip fakeSwList lSE
    
    flip mapConcurrently swsWithSes $ \(fs, stE)-> do 
      withSwitch fs "127.0.0.1" 6633 $ do
       
        let stEnt = stE
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
        
        -- thread delay example
        lift $ threadDelay 1000000 -- wait for a second

        count <- lift $ ( newIORef 0 :: IO (IORef Int))
        -- setUserHandler $ predicateHandler (\_->True) count

        -- Sending primitives:
        -- send simple packet
        -- tcp <- randomTCP
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
                                  , testPayloadLen = 32
                                  , testAckNo = Nothing
                                  , testSeqNo = Nothing
                                  , testIpID = Nothing
                                  }

        replicateM_ 100 $ do
            bid <- statsSendOFPPacketIn stEnt port pl
            return()         

        lift $ threadDelay 3000000
        lift $ putStrLn "done"
    
    stats <- assembleStats lSE 
    print stats
