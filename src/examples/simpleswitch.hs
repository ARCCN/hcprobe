{-# LANGUAGE OverloadedStrings #-}
-- | This module creates simple default switch without user program
-- that can be used for testing purposes:
--
-- This program should be as simple as possibe
module Main
  where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async -- EDSL for asynchronous actions
import Control.Monad (replicateM)
import Data.Bits                -- for IP creation [ TODO: remove ]
import HCProbe.EDSL

main = do 
    let ip = 15 .|. (0x10 `shiftL` 24) -- TODO: make ip reasonable
    fakeSw <- switch ip $ do
        features $ do
            {- replicateM 48 $ --uncomment to create 48 ports -}
            addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
            addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
        addMACs [1..450]
    print fakeSw
    race_ (runSwitch fakeSw "localhost" 6633) (threadDelay 3000000)

    
