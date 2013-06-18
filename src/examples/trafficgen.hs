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
import Data.Time

printStats :: UTCTime -> [StatsEntity] -> IO ()
printStats startTime lSE = do
                              stats <- assembleStats lSE
                              now <- getCurrentTime
                              let ctime = round $ toRational (now `diffUTCTime` startTime) :: Int
                              printf "%d %d %d\n" ctime (pktStatsLostTotal stats) (pktStatsConnLost stats)
                              threadDelay 10000000
                              printStats startTime lSE
                              
main :: IO ()
main = do 
    fakeSwList <- forM [1..5] $ \i -> do
          config $ do
                switch $ do
                    addMACs [(i*400)+1..(i+1)*400]
                    features $ do
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                      addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def

    lSE <- sequence $ map (\_->initPacketStats 1000 0.5) [1..5]

    let swsWithSes = zip fakeSwList lSE
    
    startTime <- getCurrentTime
    async $ printStats startTime lSE 
    
    flip mapConcurrently swsWithSes $ \(fs, stE)-> do 
      withSwitch fs "127.0.0.1" 6633 $ do
       
        let stEnt = stE
        setSilentStatsHandler stEnt     
        
        lift $ threadDelay 1000000 -- wait for a second

        count <- lift $ ( newIORef 0 :: IO (IORef Int))

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

        let delays = [20,25,35,60,50,40,30,25,20,15,12,10,12,14,12,10,8,7,6,7,8,10,12,15,20,20,25,30,40,50,60,35,25]
            td = 86400
        startTime <- lift getCurrentTime
        forever $ do
            bid <- statsSendOFPPacketIn stEnt port pl
            now <- lift getCurrentTime
            let ctime = round $ toRational (now `diffUTCTime` startTime) :: Int
                step = (td `div` (length delays))
                cdel = ((fromIntegral ctime) `div` step) `mod` (length delays)
                delay  = (delays !! cdel) * 10000
            lift $ threadDelay delay
            return()         

        lift $ threadDelay 3000000
        lift $ putStrLn "done"
    
    return()
