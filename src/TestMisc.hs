module Main where

import Test.HUnit
import Network.Openflow.Misc
import Network.Openflow.StrictPut
import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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
test_csum16_1 = TestCase (assertEqual "csum16_fast" (csum16' s) (csum16 s))
  where s = BS8.pack "01234567"

test_csum16_2 = TestCase (assertEqual "csum16_fast" (csum16'' s) (csum16 s))
  where s = BS8.pack "01234567"

allTests = TestList [TestLabel "putMAC#1" test_putMAC1
                    ,TestLabel "csum16#1" test_csum16_1
                    ,TestLabel "csum16#2" test_csum16_2
                    ]

main = runTestTT allTests 

