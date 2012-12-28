{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Misc
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.ARP
import Network.Openflow.Ethernet.TCP
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Messages
import Network.Openflow.Misc
import HCProbe.FakeSwitch
import HCProbe.TCP

import Data.Binary.Put ( runPut )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Data.Bits
import Text.Printf
import Data.Maybe
import Data.List (intersperse, concat, unfoldr)

import System.Random
import System.Environment (getArgs)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Concurrent.Async

mcPrefix = (0x00163e `shiftL` 24) :: Word64

testTCP = do
  dstMac <- liftM ( \x -> (x .&. 0xFFFFFF) .|. mcPrefix ) randomIO :: IO MACAddr
  srcMac <- liftM ( \x -> (x .&. 0xFFFFFF) .|. mcPrefix ) randomIO :: IO MACAddr
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

pktGenTest :: FakeSwitch -> TBMChan OfpMessage -> IO ()
pktGenTest fk chan = do
  rs <- liftM randoms newStdGen :: IO [Word32]
  forM_ rs $ \tid -> do
    threadDelay 15000
    bid <- liftM ((`mod` nbuf))    randomIO :: IO Word32
    pid <- liftM ((+1).(`mod` (nports-1)))  randomIO :: IO Word16
    tid <- randomIO :: IO Word32
    pl  <- liftM (encodePutM.putEthernetFrame) testTCP
    atomically $ writeTBMChan chan (tcpTestPkt fk tid bid pid pl)
  where nbuf = (fromIntegral.ofp_n_buffers.switchFeatures) fk
        nports = (fromIntegral.length.ofp_ports.switchFeatures) fk

tcpTestPkt fk tid bid pid pl = OfpMessage hdr (OfpPacketInReply  pktIn)
  where hdr   = header openflow_1_0 tid OFPT_PACKET_IN
        pktIn = OfpPacketIn { ofp_pkt_in_buffer_id = bid
                            , ofp_pkt_in_in_port   = pid 
                            , ofp_pkt_in_reason    = OFPR_NO_MATCH
                            , ofp_pkt_in_data      = pl
                            }
        sw  = switchFeatures fk



-- TODO: multiple switches
-- TODO: send "real" IP packets
-- TODO: generate IP packets

main :: IO ()
main = do
  (host:port:_) <- getArgs
--  let (p,g) = makePort (defaultPortGen rnd) [] [] [OFPPF_1GB_HD,OFPPF_COPPER]
--  let q = encodePutM (putOfpPort p)
--  printf "Port Len: %d\n" (BS.length q)
  workers <- forM [1..500] $ \i -> do
    rnd <- newStdGen
    let (fake@(FakeSwitch sw _),_) = makeSwitch (defaultSwGen (ipv4 10 0 0 1) rnd) 48 [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]
    let hdr = header openflow_1_0 1 OFPT_FEATURES_REPLY
    let feature_repl = OfpMessage hdr (OfpFeatureReply sw)
    let bs = bsStrict $ runPut (putMessage feature_repl)
    putStrLn (fmtSwitch sw)
    async $ ofpClient pktGenTest fake (BS8.pack host) (read port)
  mapM_ wait workers
  putStrLn "done"


