{-# LANGUAGE OverloadedStrings #-}
-- | This module creates simple default switch without user program
-- that can be used for testing purposes:
--
-- This program should be as simple as possibe
module Main
  where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_) -- EDSL for asynchronous actions
import Data.Bits                        -- for IP creation [ TODO: remove ]
import HCProbe.EDSL

main = do 
    let ip = 15 .|. (0x10 `shiftL` 24) -- TODO: make ip reasonable
    (sw1,sw2) <- config $ do
      sw1 <- switch ip $ do
            features $ do
                {- replicateM 48 $ --uncomment to create 48 ports -}
                addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
                addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
            addMACs [1..450]
      sw2 <- switchOn sw1 $ do
                clearMACs 
                addMACs [400..500]
      return (sw1,sw2)
    print sw2
    race_ (runSwitch sw1 "localhost" 6633) (threadDelay 3000000)

    
