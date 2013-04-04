{-# Language OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Word
import Network.Openflow.StrictPut
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Blaze.ByteString.Builder -- import Data.ByteString.Lazy.Builder 

import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Misc
import HCProbe.ARP
import HCProbe.TCP
import HCProbe.FakeSwitch (mcPrefix)


import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
import Test.QuickCheck hiding (Result)

pktNum = 1000000

testBuildFrame :: EthernetFrame a => a -> BS.ByteString
testBuildFrame = BS.concat . LBS.toChunks . toLazyByteString . buildEthernetFrame
testPutFrame :: EthernetFrame a => a -> BS.ByteString
testPutFrame   = runPutToByteString 32768 . putEthernetFrame

instance Arbitrary ARPGratuitousReply where
        arbitrary = ARPGratuitousReply <$> arbitrary
                                       <*> arbitrary

testARP :: ARPGratuitousReply -> Result
testARP a = testBuildFrame a ==? testPutFrame a 

data TestEthernetFrame = TestEthernetFrame !Int !MACAddr !MACAddr
                       deriving (Show)
instance Arbitrary TestEthernetFrame where
        arbitrary = TestEthernetFrame <$> (fmap (`mod` 32700) arbitrary)
                                      <*> arbitrary
                                      <*> arbitrary

instance EthernetFrame TestEthernetFrame where
  dstMacAddress  (TestEthernetFrame _ a _) = a
  {-# INLINE dstMacAddress #-}
  srcMacAddress (TestEthernetFrame _ _ b)  = b 
  {-# INLINE srcMacAddress #-}
  vlanID         = const Nothing
  {-# INLINE vlanID #-}
  typeCode       = const 0x806
  {-# INLINE typeCode #-}
  putPayload  (TestEthernetFrame n _ _)  = putEmptyPayload n 
  {-# INLINE putPayload #-}

testPkg :: TestEthernetFrame -> Result
testPkg a = testBuildFrame a ==? testPutFrame a

instance Arbitrary TestPacketTCP where
        arbitrary = do
                    n <- (`mod` 32700) <$> arbitrary
                    TestPacketTCP <$> arbitrary -- mac0
                                  <*> arbitrary -- mac1
                                  <*> arbitrary -- ip0
                                  <*> arbitrary -- ip1
                                  <*> arbitrary -- port0
                                  <*> arbitrary -- port1
                                  <*> arbitrary -- ip id
                                  <*> arbitrary -- seq no
                                  <*> arbitrary -- ack no
                                  <*> arbitrary -- wss
                                  <*> arbitrary -- (pure ack)-- ack
                                  <*> pure n
                                  <*> (BS.pack <$> vectorOf n arbitrary)

testTCP :: TestPacketTCP -> Result
testTCP x = testBuildFrame x ==? testPutFrame x

main = hspec $ describe "bytestring equal to strict put" $ do
          prop "arp" $ testARP
          prop "pkg" $ testPkg
          prop "tcp" $ testTCP

