module Main where

import Test.HUnit

import Nettle.OpenFlow.StrictPut
import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import qualified Data.ByteString as BS
import qualified Data.Binary.Put as BP
import Data.Binary.Get

test_putMAC1 = TestCase (assertEqual "macEqual" macStrict macBS)
  where macStrict = runPutToByteString 16 $ putMAC 0xFFFFAABBCCDDEEFF
        macBS     = bsStrict $ BP.runPut $ do BP.putWord8 0xAA
                                              BP.putWord8 0xBB
                                              BP.putWord8 0xCC
                                              BP.putWord8 0xDD
                                              BP.putWord8 0xEE
                                              BP.putWord8 0xFF

allTests = TestList [TestLabel "putMAC#1" test_putMAC1
                    ]

main = runTestTT allTests 


