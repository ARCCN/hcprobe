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

import Debug.Trace

encodeMsg = encodePutM . putMessage

-- TODO: move to FakeSwitch ?

data SwitchState = SwitchState { swTranID :: Word32
                               , swCfg :: OfpSwitchConfig
                               }


data SwitchContext = SwitchContext { handshakeDone :: TVar Bool
                                   }

pktSendTimeout = 500000

ofpClient sw host port = do
  switchCfg <- newTVarIO (SwitchState 0 defaultSwitchConfig)
  runTCPClient (clientSettings port host) (client sw switchCfg)

client fk@(FakeSwitch sw switchIP) cfg ad = do 

  runResourceT $ do
  -- TODO: allocate shared structures
  -- TODO: forkIO receiver
  -- TODO: forkIO sender

    (_, pktSendQ) <- allocate (newTBMChanIO 512) (atomically.closeTBMChan)
    featureReplyMonitor <- liftIO $ newTVarIO False

    let ctx = SwitchContext featureReplyMonitor

    let sender = forever $ do
        withTimeout pktSendTimeout (readTVar featureReplyMonitor >>= flip unless retry)
        v <- liftIO $ atomically (readTBMChan pktSendQ)
        case v of 
          Just msg -> sendReplyT msg (return ())
          Nothing  -> return ()
        return ()

    let receiver = appSource ad $$ forever $ do
        bs' <- await
        when (isJust bs') $ do
          let bs = fromJust bs'
          case (ofpParsePacket bs) of
            Just (msg, rest) -> (liftIO $ dump "IN:" (ofp_header msg) bs) >> dispatch ctx msg >> leftover rest
            Nothing          -> return ()    

    let sendARPGrat = do
        withTimeout pktSendTimeout (readTVar featureReplyMonitor >>= flip unless retry)
        tid <- nextTranID
        sendReplyT (arpGrat tid) (return ())

    allocate (M.forkIO (sender)) (M.killThread)
    allocate (M.forkIO (sendARPGrat)) (M.killThread)
    allocate (M.forkIO (receiver)) (M.killThread)

    forever $ do liftIO $ M.yield

  where
    sendReplyT msg fm = fm >> do
      liftIO $ dump "OUT:" (ofp_header msg) replyBs
      yield replyBs $$ (appSink ad)
      where replyBs = encodeMsg msg

    dispatch c msg@(OfpMessage hdr msgData) = case (parseMessageData msg) of
      Nothing   -> return ()
      Just msg' -> processMessage c (ofp_hdr_type hdr) msg'

    processMessage _ OFPT_HELLO (OfpMessage hdr _) = sendReply (headReply hdr OFPT_HELLO) nothing

    processMessage c OFPT_FEATURES_REQUEST (OfpMessage hdr msg) = sendReply reply nothing 
      where reply = featuresReply openflow_1_0 sw (ofp_hdr_xid hdr)

    processMessage _ OFPT_ECHO_REQUEST (OfpMessage hdr (OfpEchoRequest payload)) = sendReply reply nothing
      where reply = echoReply openflow_1_0 payload (ofp_hdr_xid hdr)

    processMessage _ OFPT_SET_CONFIG (OfpMessage hdr (OfpSetConfig cfg')) = do
      liftIO $ atomically $ do 
        st <- readTVar cfg
        writeTVar cfg (st { swCfg = cfg' })

    processMessage _ OFPT_GET_CONFIG_REQUEST (OfpMessage hdr msg) = do
      st <- liftIO $ atomically $ readTVar cfg
      sendReply (getConfigReply hdr (swCfg st)) nothing

    -- FIXME: possible problems with other controllers rather than NOX
    processMessage c OFPT_BARRIER_REQUEST msg = do
      -- TODO: do something, process all pkts, etc
      sendReply (headReply (ofp_header msg) OFPT_BARRIER_REPLY) $ do
        liftIO $ atomically (writeTVar (handshakeDone c) True)

    processMessage _ OFPT_VENDOR msg = do
      let errT = OfpError (OFPET_BAD_REQUEST OFPBRC_BAD_VENDOR) (BS.empty)
      let reply = errorReply (ofp_header msg) errT
      sendReply (reply) nothing

    -- TODO: implement the following messages
    processMessage _ OFPT_PACKET_OUT m@(OfpMessage hdr msg) = do
      nothing

    -- TODO: implement the following messages
    processMessage _ OFPT_FLOW_MOD (OfpMessage hdr msg) = nothing
    processMessage _ OFPT_STATS_REQUEST (OfpMessage hdr msg) = nothing

    processMessage _ _ _ = nothing

    nothing = return ()

    sendReply msg fm = fm >> do
      liftIO $ dump "OUT:" (ofp_header msg) replyBs
      lift $ yield replyBs $$ (appSink ad)
      where replyBs = encodeMsg msg

    arpGrat tid   = OfpMessage hdr (OfpPacketInReply  pktIn)
      where hdr   = header openflow_1_0 tid OFPT_PACKET_IN
            pktIn = OfpPacketIn { ofp_pkt_in_buffer_id = (1 :: Word32)
                                , ofp_pkt_in_in_port   = defaultPacketInPort
                                , ofp_pkt_in_reason    = OFPR_NO_MATCH
                                , ofp_pkt_in_data      = arpGratData 
                                }
            arpGratData = encodePutM $ putEthernetFrame (ARPGratuitousReply switchMAC switchIP)
            switchMAC = ofp_datapath_id sw

--    nextTranID :: Application IO
    nextTranID = liftIO $ atomically $ do 
      st@(SwitchState t c) <- readTVar cfg
      writeTVar cfg (st{swTranID = succ t})
      return (fromIntegral t)

    defaultPacketInPort = (ofp_port_no . last . ofp_ports) sw

    withTimeout :: Int -> (STM a) -> IO (Maybe a)
    withTimeout tv f = do
      x <- registerDelay tv
      atomically $ (readTVar x >>= \y -> if y
                                         then return Nothing
                                         else retry) `orElse` (Just <$> f)

-- TODO: move liftIO here
-- TODO: truncate message by length in header
-- TODO: use logger / settings
dump :: String -> OfpHeader -> BS.ByteString -> IO ()
dump s hdr bs = do
  let tp = show (ofp_hdr_type hdr)
  putStr $ printf "%-4s %-24s %s\n" s tp (hexdumpBs 32 " " "" (BS.take 32 bs))

defActions = [ OFPAT_OUTPUT,OFPAT_SET_VLAN_VID,OFPAT_SET_VLAN_PCP
             , OFPAT_STRIP_VLAN,OFPAT_SET_DL_SRC,OFPAT_SET_DL_DST
             , OFPAT_SET_NW_SRC,OFPAT_SET_NW_DST,OFPAT_SET_NW_TOS
             , OFPAT_SET_TP_SRC,OFPAT_SET_TP_DST
             ]

main :: IO ()
main = do
  (host:port:_) <- getArgs
  rnd <- newStdGen
--  let (p,g) = makePort (defaultPortGen rnd) [] [] [OFPPF_1GB_HD,OFPPF_COPPER]
--  let q = encodePutM (putOfpPort p)
--  printf "Port Len: %d\n" (BS.length q)
  let (fake@(FakeSwitch sw _),g') = makeSwitch (defaultSwGen (ipv4 10 0 0 1) rnd) 48 [] defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]
  let hdr = header openflow_1_0 1 OFPT_FEATURES_REPLY
  let feature_repl = OfpMessage hdr (OfpFeatureReply sw)
  let bs = bsStrict $ runPut (putMessage feature_repl)
  putStrLn (fmtSwitch sw)
  ofpClient fake (BS8.pack host) (read port)
  putStrLn "done"


