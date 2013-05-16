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
import Network.Openflow.Messages
import Network.Openflow.Misc
import Data.Binary (Binary(..))
import HCProbe.Ethernet
import qualified Network.Openflow.StrictPut as SP
import Data.Binary.Put
import Data.Binary.Put ( runPut )
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import HCProbe.EDSL.Handlers
import Data.IORef
import Data.Word

putData (OfpPortStatus (OfpPortStatusData reason data_)) = do
    SP.putWord8 . fromIntegral . fromEnum $ reason
    SP.putZeros 7
    putPort data_

--putPort :: OfpPhyPort -> PutM ()
putPort port = do
  SP.putWord16be (ofp_port_no port)                                                            -- 2
  mapM_ SP.putWord8 (drop 2 (unpack64 (ofp_port_hw_addr port)))                                -- 8
  putASCII 16 (ofp_port_name port)    -- ASCIIZ(16)
  SP.putWord32be $ ofp_port_config port      --(bitFlags ofConfigFlags (ofp_port_config port))
  SP.putWord32be $ ofp_port_state port       --(bitFlags ofStateFlags (ofp_port_state port))
  SP.putWord32be $ ofp_port_current port     --(bitFlags ofFeatureFlags (ofp_port_current port))
  SP.putWord32be $ ofp_port_advertised port  --(bitFlags ofFeatureFlags (ofp_port_advertised port))
  SP.putWord32be $ ofp_port_supported port   --(bitFlags ofFeatureFlags (ofp_port_supported port))
  SP.putWord32be $ ofp_port_peer port        --(bitFlags ofFeatureFlags (ofp_port_peer port))

putASCII sz bs = SP.putByteString bs' >> replicateM_ (sz - (BS.length bs')) (SP.putWord8 7)
  where bs' = BS.take (sz - 1) bs

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

	-- broken ascii
	replicateM_ 100 $ do
            --x <- nextBID
            --lift . putStrLn $ "next buffer id " ++ show x
            let msg = putOFMessage $ do
                  putOFHeader $ do
                    putHdrType OFPT_PORT_STATUS
                    putHdrLength 8
                  putRaw $ SP.runPutToByteString 4096 $ putData (OfpPortStatus (OfpPortStatusData OFPR_ADD (head.ofp_ports.eSwitchFeatures $ fakeSw)))
            send msg
            lift $ threadDelay 10000
	
	lift $ threadDelay 1000000

	lift $ putStrLn "done"
    
    stats <- assembleStats lSE 
    print stats
