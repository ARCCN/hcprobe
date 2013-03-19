{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Messages
import Network.Openflow.Misc
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.ARP
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import HCProbe.FakeSwitch
import HCProbe.ARP
import HCProbe.TCP

import Data.Bits
import Data.Binary.Strict.Get
import Data.Maybe
import Data.Either
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Data.Maybe
import Control.Monad
import Text.Printf

import Nettle.OpenFlow.StrictPut

import System.Random
import System.Environment (getArgs)
import Debug.Trace

testTCP = do
  dstMac <- liftM mcPrefix randomIO :: IO MACAddr
  srcMac <- liftM mcPrefix randomIO :: IO MACAddr
  srcIp  <- randomIO :: IO IPv4Addr
  dstIp  <- randomIO :: IO IPv4Addr
  srcP   <- randomIO :: IO Word16
  dstP   <- randomIO :: IO Word16
  wss    <- randomIO :: IO Int 
  flags  <- return [ACK]
  cargo  <- replicateM 128 randomIO :: IO [Word8]
  return $ TestPacketTCP { dstMAC = dstMac
                         , srcMAC = srcMac
                         , srcIP  = srcIp
                         , dstIP  = dstIp
                         , dstPort = dstP
                         , srcPort = srcP
                         , testWSS = Just wss
                         , testFlags = Just flags
                         , payLoad = BS.pack cargo
                         , testSeqNo = Nothing
                         , testAckNo = Nothing
                         , testIpID = Nothing
                         }


crcTest :: IO ()                         
crcTest = replicateM_ 1000 $ do
  pkt <- replicateM 64 randomIO >>= return . BS.pack
  let crc = csum16 pkt
  printf "%s %s\n" (show crc) (hexdumpBs 64 "" "" pkt)


crcTest2 :: IO ()
crcTest2 = do
  let w8 = [0x4500, 0x003c, 0x1c46, 0x4000, 0x4006, 0xb1e6, 0xac10, 0x0a63, 0xac10, 0x0a0c] :: [Word16]
  let c = (foldl' ( (+) . complement) 0) w8
  printf "%04X\n" c

crcTest3 :: IO ()
crcTest3 = do
  let w8 = [ 0x45, 0x00, 0x00, 0x3c, 0x1c, 0x46
           , 0x40, 0x00, 0x40, 0x06, 0xb1, 0xe6
           , 0xa, 0xc10, 0x0a, 0x63, 0xac, 0x10
           , 0x0a, 0x0c] :: [Word8]

  let c = csum16 (BS.pack w8)
  printf "%04X\n" (fromJust c)

crcTest4 :: IO ()
crcTest4 = do
  let w8 = [ 0x45, 0x00, 0x00, 0x3c, 0x1c, 0x46
           , 0x40, 0x00, 0x40, 0x06, 0xb1, 0xe6
           , 0xa, 0xc10, 0x0a, 0x63, 0xac, 0x10
           , 0x0a, 0x0c] :: [Word8]
  let cs = fromJust $ checksum (BS.pack w8)
  return ()


crcTest5 :: IO ()
crcTest5 = do
  let pkt = [0x50, 0x86, 0x2D, 0x61, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x10, 0x58, 
             0xE4, 0x00, 0x00, 0x00, 0x00, 0x16, 0x42, 0x2E, 0x44, 0x62, 0x17, 0xF2, 0x86, 0x4D, 0xB9, 
             0x3E, 0x45, 0xB2, 0xED, 0x88, 0x3C, 0xEC, 0x53, 0x19, 0x73, 0xBF, 0xAC, 0x88, 0x05, 0xB0, 
             0x11, 0x83, 0x01, 0xC3, 0xDA, 0x7B, 0xCC, 0x67, 0x0A, 0x7B, 0xD6, 0x1E, 0x8E, 0xCB, 0x4F, 
             0x47, 0x66, 0x24, 0x69, 0x46, 0x30, 0x02, 0x2B, 0x82, 0x9A, 0x12, 0x21, 0x1E, 0xA1, 0xAA, 
             0x52, 0x36, 0xA0, 0xC9, 0x3E, 0x98, 0xDB, 0xA5, 0xFC, 0x0B, 0xA1, 0x0C, 0xDA, 0x69, 0x0B, 
             0xEE, 0x7A, 0x16, 0xB2, 0x3E, 0xDE, 0x4E, 0xB8, 0xC5, 0x09, 0x11, 0x10, 0xFC, 0x23, 0x41, 
             0x4A, 0x64, 0x33, 0x76, 0xBF, 0x7E, 0x84, 0x17, 0x9F, 0x15, 0x31, 0x0F, 0x86, 0x1C, 0xC9, 
             0x26, 0x43, 0xC0, 0xFF, 0x4A, 0xC9, 0xAC, 0x44, 0xE4, 0x35, 0x62, 0xBD, 0xBA, 0xF9, 0xC7, 
             0x7A, 0x96, 0x9D, 0xBF, 0x72, 0xE6, 0x3F, 0xA2, 0x79, 0x0D, 0x41, 0xC7, 0xDA
            ]
  let cs = fromJust $ checksum (BS.pack pkt)
  printf "%04X\n" cs
  return ()

checksum :: BS.ByteString -> Maybe Word16
checksum s = words >>= return . trunc . (foldl' (\acc w -> trace (printf "%08X\n" acc) $ acc + fromIntegral w) 0)
  where withResult (Left _, _)  = Nothing
        withResult (Right s, _) = Just s 
        words = withResult $ flip runGet s (replicateM (BS.length s `div` 2) $ getWord16le)
        trunc :: Word32 -> Word16
        trunc w = fromIntegral $ complement $ (w .&. 0xFFFF) + (w `shiftR` 16)

main = do
--  crcTest5
  tcp <- testTCP
  let bs = runPutToByteString 2048 $ putEthernetFrame tcp
--  let bs = bsStrict $ runPut $ putEthernetFrame (ARPGratuitousReply 0 0)
--  let bs = bsStrict $ runPut $ putEthernetFrame (ARPGratuitousReply 0 0)  
  putStr (hexdumpBs 16 " " "\n" bs)
