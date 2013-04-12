{-# LANGUAGE OverloadedStrings #-}
-- | This module creates simple default switch without user program
-- that can be used for testing purposes:
--
-- This program should be as simple as possibe
module Main
  where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans
import Data.Bits                -- for IP creation [ TODO: remove ]
import HCProbe.EDSL

main :: IO ()
main = do 
    let ip = 15 .|. (0x10 `shiftL` 24) -- TODO: make ip reasonable
    fakeSw <- switch ip $ do
                features $ do
                  addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                  addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                addMACs [1..450]
    print fakeSw
    withSwitch fakeSw "localhost" 6633 $ do
        -- await forever
        lift $ putStr "waiting for barrier request.. "
        waitForType OFPT_BARRIER_REQUEST
        lift $ putStrLn  "[done]"
        lift $ putStr "waiting for echo request.. "
        waitForType OFPT_ECHO_REQUEST
        lift $ putStrLn "[done]"
        
        lift $ putStr "waiting for 1 second.. "
        lift $ threadDelay 1000000 -- wait for a second
        lift $ putStrLn "[done]"
        
        -- Sending primitives:
        -- send simple packet
        -- tcp <- randomTCP
        -- bid <- sendOFPPacket (simplePacketIn payload)

        {-
        -- wait for responce
        waitForOFAnswer bid
        -}
