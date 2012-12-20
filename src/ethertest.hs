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
import Data.Binary.Put
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

import System.Random
import System.Environment (getArgs)
import Debug.Trace


testTCP = do
  dstMac <- randomIO :: IO MACAddr
  srcMac <- randomIO :: IO MACAddr
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
  printf "%08X\n" cs
  return ()


checksum :: BS.ByteString -> Maybe Word16
checksum s = words >>= return . trunc . (foldl' (\acc w -> acc + fromIntegral w) 0)
  where withResult (Left _, _)  = Nothing
        withResult (Right s, _) = Just s 
        words = withResult $ flip runGet s (replicateM (BS.length s `div` 2) $ getWord16be)
        trunc :: Word32 -> Word16
        trunc w = fromIntegral $ complement w -- $ (w .&. 0xFFFF) + (w `shiftR` 16)

main = do
  tcp <- testTCP
  let bs = bsStrict $ runPut $ putEthernetFrame tcp
  putStr (hexdumpBs 16 " " "\n" bs)
