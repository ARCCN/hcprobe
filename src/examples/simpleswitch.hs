{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async -- EDSL for asynchronous actions
import Control.Monad (replicateM)
import Data.Bits                -- for IP creation [ TODO: remove ]
import Data.Default             -- for def [TODO: remove]
import HCProbe.EDSL
import HCProbe.FakeSwitch       -- low level access [ TODO: remove in simple tasks ]
import Network.Openflow.Types   -- OOFFPF [TODO: remove]
import Data.ByteString.Char8 as BS8 -- for BS8.pack [TODO: remove]

main = do 
    let ip = 15 .|. (0x10 `shiftL` 24)
        macs = []
        fakeSw = switch ip $ do
            features $ do
              {-replicateM 48 $-}
              addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
              addPort [] [] [OFPPF_1GB_FD, OFPPF_COPPER] def
            addMACs [1..450]
    print fakeSw
    race_ (runSwitch fakeSw "localhost" 6633) (threadDelay 3000000)

    
