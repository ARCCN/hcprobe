{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Ethernet.ARP
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Messages
import Network.Openflow.Misc
import HCProbe.FakeSwitch
import HCProbe.ARP

import Data.Binary.Put ( runPut )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Text.Printf
import Data.Maybe
import Data.List (intersperse, concat, unfoldr)

import System.Random
import System.Environment (getArgs)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Conduit.Network

import qualified Control.Concurrent as M
import Control.Monad.Trans.Resource hiding (runResourceT)
import Control.Monad.STM
import Control.Concurrent.STM

import Control.Applicative ((<$>))

import Control.Concurrent.STM.TBMChan

import Control.Monad.Maybe

import Debug.Trace

encodeMsg = encodePutM . putMessage

-- TODO: move to FakeSwitch ?

data SwitchContext = SwitchContext { handshakeDone :: TVar Bool
                                   , transactionID :: TVar Int
                                   , switchCfg     :: TVar OfpSwitchConfig
                                   }

pktSendTimeout = 500000
pktSendQLen    = 10000

-- FIXME: handle "resource vanished" exception
ofpClient pktGen sw host port = do
  runTCPClient (clientSettings port host) (client pktGen sw)

client pktInGen fk@(FakeSwitch sw switchIP) ad = do

  runResourceT $ do
  -- TODO: allocate shared structures
  -- TODO: forkIO receiver
  -- TODO: forkIO sender

    (_, pktSendQ) <- allocate (newTBMChanIO pktSendQLen) (atomically.closeTBMChan)

    tranId <- liftIO $ newTVarIO 0
    featureReplyMonitor <- liftIO $ newTVarIO False
    swCfg <- liftIO $ newTVarIO defaultSwitchConfig

    let ctx = SwitchContext featureReplyMonitor tranId swCfg

    let sender = forever $ do
        withTimeout pktSendTimeout (readTVar featureReplyMonitor >>= flip unless retry)
        liftIO $ atomically (readTBMChan pktSendQ) >>= maybe skip sendReplyT
        where skip = return ()

    let receiver = appSource ad $$ forever $ runMaybeT $ do
        bs <- MaybeT await
        (msg, rest) <- MaybeT $ return (ofpParsePacket bs)
        lift $ liftIO (dump "IN:" (ofp_header msg) bs) >> dispatch ctx msg >> leftover rest

    let sendARPGrat = do
        withTimeout pktSendTimeout (readTVar featureReplyMonitor >>= flip unless retry)
        liftM (arpGrat fk (-1 :: Word32)) (nextTranID ctx) >>= sendReplyT


    let mainThread = receiver
    let threads = [sender, sendARPGrat, (pktInGen fk pktSendQ)]
    mapM_ ((flip allocate M.killThread) . M.forkIO) threads

    liftIO mainThread

  where
    sendReplyT msg = do
      liftIO $ dump "OUT:" (ofp_header msg) replyBs
      yield replyBs $$ (appSink ad)
      where replyBs = encodeMsg msg

    dispatch c msg@(OfpMessage hdr msgData) = case (parseMessageData msg) of
      Nothing   -> return ()
      Just msg' -> processMessage c (ofp_hdr_type hdr) msg'

    processMessage _ OFPT_HELLO (OfpMessage hdr _) = sendReply (headReply hdr OFPT_HELLO)

    processMessage c OFPT_FEATURES_REQUEST (OfpMessage hdr msg) = sendReply reply
      where reply = featuresReply openflow_1_0 sw (ofp_hdr_xid hdr)

    processMessage _ OFPT_ECHO_REQUEST (OfpMessage hdr (OfpEchoRequest payload)) = sendReply reply
      where reply = echoReply openflow_1_0 payload (ofp_hdr_xid hdr)

    processMessage c OFPT_SET_CONFIG (OfpMessage hdr (OfpSetConfig cfg')) = do
      liftIO $ atomically $ modifyTVar (switchCfg c) (const cfg')

    processMessage c OFPT_GET_CONFIG_REQUEST (OfpMessage hdr msg) =
      (liftIO $ atomically $ readTVar (switchCfg c)) >>= sendReply.getConfigReply hdr

    -- FIXME: possible problems with other controllers rather than NOX
    processMessage c OFPT_BARRIER_REQUEST msg = do
      -- TODO: do something, process all pkts, etc
      liftIO $ atomically (writeTVar (handshakeDone c) True)
      sendReply (headReply (ofp_header msg) OFPT_BARRIER_REPLY)

    processMessage _ OFPT_VENDOR msg = do
      let errT = OfpError (OFPET_BAD_REQUEST OFPBRC_BAD_VENDOR) BS.empty
      let reply = errorReply (ofp_header msg) errT
      sendReply reply

    -- TODO: implement the following messages
    processMessage _ OFPT_PACKET_OUT m@(OfpMessage hdr msg) =
      nothing

    -- TODO: implement the following messages
    processMessage _ OFPT_FLOW_MOD (OfpMessage hdr msg) = nothing
    processMessage _ OFPT_STATS_REQUEST (OfpMessage hdr msg) = nothing

    processMessage _ _ _ = nothing

    nothing = return ()

    sendReply msg = do
      liftIO $ dump "OUT:" (ofp_header msg) replyBs
      lift $ yield replyBs $$ appSink ad
      where replyBs = encodeMsg msg

    nextTranID c = liftIO $ atomically $ do
      modifyTVar (transactionID c) succ
      readTVar (transactionID c) >>= return . fromIntegral

    withTimeout :: Int -> STM a -> IO (Maybe a)
    withTimeout tv f = do
      x <- registerDelay tv
      atomically $ (readTVar x >>= \y -> if y
                                         then return Nothing
                                         else retry) `orElse` (Just <$> f)

-- FIXME: last raises exception on empty list
defaultPacketInPort = ofp_port_no . last . ofp_ports

arpGrat fk bid tid   = OfpMessage hdr (OfpPacketInReply  pktIn)
  where hdr   = header openflow_1_0 tid OFPT_PACKET_IN
        pktIn = OfpPacketIn { ofp_pkt_in_buffer_id = bid
                            , ofp_pkt_in_in_port   = defaultPacketInPort sw
                            , ofp_pkt_in_reason    = OFPR_NO_MATCH
                            , ofp_pkt_in_data      = arpGratData 
                            }
        arpGratData = encodePutM $ putEthernetFrame (ARPGratuitousReply mac ip)
        mac = ofp_datapath_id sw
        ip  = switchIP fk 
        sw  = switchFeatures fk


-- TODO: move liftIO here
-- TODO: truncate message by length in header
-- TODO: use logger / settings
dump :: String -> OfpHeader -> BS.ByteString -> IO ()
--dump s hdr bs = return ()
dump s hdr bs = do
  let tp = show (ofp_hdr_type hdr)
  putStr $ printf "%-4s %-24s %s\n" s tp (hexdumpBs 32 " " "" (BS.take 32 bs))

defActions = [ OFPAT_OUTPUT,OFPAT_SET_VLAN_VID,OFPAT_SET_VLAN_PCP
             , OFPAT_STRIP_VLAN,OFPAT_SET_DL_SRC,OFPAT_SET_DL_DST
             , OFPAT_SET_NW_SRC,OFPAT_SET_NW_DST,OFPAT_SET_NW_TOS
             , OFPAT_SET_TP_SRC,OFPAT_SET_TP_DST
             ]

pktGenTest :: FakeSwitch -> TBMChan OfpMessage -> IO ()
pktGenTest fk chan = do
  rs <- liftM randoms newStdGen :: IO [Word32]
  forM_ rs $ \tid -> do
    M.threadDelay 150000
    atomically $ writeTBMChan chan (arpGrat fk 1 tid)

main :: IO ()
main = do
  (host:port:_) <- getArgs
  rnd <- newStdGen
--  let (p,g) = makePort (defaultPortGen rnd) [] [] [OFPPF_1GB_HD,OFPPF_COPPER]
--  let q = encodePutM (putOfpPort p)
--  printf "Port Len: %d\n" (BS.length q)
  let (fake@(FakeSwitch sw _),_) = makeSwitch (defaultSwGen (ipv4 10 0 0 1) rnd) 48 [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]
  let hdr = header openflow_1_0 1 OFPT_FEATURES_REPLY
  let feature_repl = OfpMessage hdr (OfpFeatureReply sw)
  let bs = bsStrict $ runPut (putMessage feature_repl)
  putStrLn (fmtSwitch sw)
  ofpClient pktGenTest fake (BS8.pack host) (read port)
  putStrLn "done"


