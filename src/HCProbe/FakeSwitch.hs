module HCProbe.FakeSwitch ( PortGen(..), FakeSwitch(..)
                          , makePort
                          , makeSwitch
                          , defaultPortGen
                          , defaultSwGen
                          , fmtMac, fmtPort, fmtSwitch
                          , ofpClient
                          , arpGrat
                          , defActions
                          , defaultPacketInPort
                          , encodeMsg
                          ) where

import HCProbe.ARP
import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Network.Openflow.Ethernet.Generator
import Network.Openflow.Messages
import Network.Openflow.Misc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as S
import Control.Applicative ((<$>))
import qualified Control.Concurrent as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.State
import Control.Monad.STM
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Conduit
import Data.Conduit.Network
import Data.List
import Data.Maybe
import Data.Word
import System.Random
import Text.Printf

import Debug.Trace

data PortGen = PortGen { pnum   :: Int
                       , pname  :: Int -> BS.ByteString
                       , rndGen :: StdGen
                       }

defaultPortGen :: StdGen -> PortGen
defaultPortGen g = PortGen 1 (BS8.pack.printf "eth%d" . pred) g

makePort :: PortGen
         -> [OfpPortConfigFlags] 
         -> [OfpPortStateFlags]
         -> [OfpPortFeatureFlags]
         -> (OfpPhyPort, PortGen)

makePort gen cfg st ft = (port, gen') 
  where pn  = pnum gen
        pnm = (pname gen) pn
        gen' = gen { pnum = pn + 3, rndGen = (snd.head.reverse) mac' }
        mac' = take 3 $ unfoldr ( \g -> Just (rand g, snd (rand g)) ) (rndGen gen)
        macbytes =  [0x00, 0x16, 0x3e] ++ map fst mac' :: [Word8]
        fmac acc b = (acc `shiftL` 8) .|. (fromIntegral b :: Word64)
        rand :: StdGen -> (Word8, StdGen)
        rand = random
        port = OfpPhyPort { ofp_port_no         = fromIntegral pn
                          , ofp_port_hw_addr    = foldl fmac 0 macbytes
                          , ofp_port_name       = pnm
                          , ofp_port_config     = S.fromList cfg
                          , ofp_port_state      = S.fromList st
                          , ofp_port_current    = S.fromList ft
                          , ofp_port_advertised = S.fromList ft
                          , ofp_port_supported  = S.fromList ft
                          , ofp_port_peer       = S.fromList ft
                          }

data SwitchGen = SwitchGen {  dpid    :: Int
                           ,  ipAddr  :: IPv4Addr
                           ,  swRnd   :: StdGen
                           }
defaultSwGen :: Int -> IPv4Addr -> StdGen -> SwitchGen
defaultSwGen i ip g = SwitchGen i ip g

data FakeSwitch = FakeSwitch {  switchFeatures :: OfpSwitchFeatures
                              , switchIP       :: IPv4Addr 
                              , onSendMessage  :: Maybe (OfpMessage -> IO ())
                              , onRecvMessage  :: Maybe (OfpMessage -> IO ())
                             }

makeSwitch :: SwitchGen
           -> Int
           -> [OfpCapabilities]
           -> [OfpActionType]
           -> [OfpPortConfigFlags]
           -> [OfpPortStateFlags]
           -> [OfpPortFeatureFlags]
           -> (FakeSwitch, SwitchGen)

makeSwitch gen ports cap act cfg st ff = (FakeSwitch features (ipAddr gen) Nothing Nothing, gen')
  where features = OfpSwitchFeatures { ofp_datapath_id  = fromIntegral (dpid gen)
                                     , ofp_n_buffers    = fromIntegral $ 8*ports
                                     , ofp_n_tables     = 1
                                     , ofp_capabilities = S.fromList cap
                                     , ofp_actions      = S.fromList act
                                     , ofp_ports        = pps
                                     }
        gen' = gen { dpid = succ (dpid gen), swRnd = (rndGen pg') }
        (pps, pg') = flip runState pg $ replicateM ports genPort
        pg = defaultPortGen (swRnd gen)

        genPort = do
          g <- get
          let (p,g') = makePort g cfg st ff
          put g'
          return p

fmtMac :: MACAddr -> String
fmtMac mac = intercalate ":" $ map (printf "%02X") bytes
  where bytes = drop 2 $ unpack64 mac

fmtPort :: OfpPhyPort -> String
fmtPort p = printf "%02d %-6s HWAddr:%18s, ST: %s, FT: %s" pno pname mac st ft
  where pno = ofp_port_no p
        pname = BS8.unpack (ofp_port_name p)
        mac = fmtMac (ofp_port_hw_addr p)
        st  = show $ S.toList (ofp_port_state p)
        ft  = show $ S.toList (ofp_port_current p)

fmtSwitch :: OfpSwitchFeatures -> String
fmtSwitch f = printf "DPID: %s, %s\n" dp cap ++ intercalate "\n" ports
  where dp  = fmtMac (ofp_datapath_id f)
        cap = show (S.toList (ofp_capabilities f))
        ports = map fmtPort (ofp_ports f) 

encodeMsg = encodePutM . putMessage

data SwitchContext = SwitchContext { handshakeDone :: TVar Bool
                                   , transactionID :: TVar Int
                                   , switchCfg     :: TVar OfpSwitchConfig
                                   }

pktSendTimeout = 500000
pktSendQLen    = 10000

-- FIXME: handle "resource vanished" exception
ofpClient pktGen sw host port = do
  runTCPClient (clientSettings port host) (client pktGen sw)

client pktInGen fk@(FakeSwitch sw switchIP sH rH) ad = do

  runResourceT $ do

    (_, pktSendQ) <- allocate (newTBMChanIO pktSendQLen) (atomically.closeTBMChan)

    tranId <- liftIO $ newTVarIO 0
    featureReplyMonitor <- liftIO $ newTVarIO False
    swCfg <- liftIO $ newTVarIO defaultSwitchConfig

    let ctx = SwitchContext featureReplyMonitor tranId swCfg

    let sender = forever $ do
        withTimeout pktSendTimeout (readTVar featureReplyMonitor >>= flip unless retry)
        liftIO $! atomically (readTBMChan pktSendQ) >>= maybe skip sendReplyT
        where skip = return ()

    let receiver = appSource ad $$ forever $ runMaybeT $ do
        bs <- await
--        (msg, rest) <- MaybeT $! return (ofpParsePacket bs)
--        lift $ liftIO (dump "IN:" (ofp_header msg) bs) >> dispatch ctx msg >> leftover rest

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
--      maybe (return ()) (\x -> (liftIO.x) msg) sH
      where replyBs = encodeMsg msg

    dispatch c msg@(OfpMessage hdr msgData) = case (parseMessageData msg) of
      Nothing   ->  return ()
      Just msg'@(OfpMessage h _) -> processMessage c (ofp_hdr_type hdr) msg'

    -- TODO: implement the following messages
    processMessage _ OFPT_PACKET_OUT m@(OfpMessage hdr msg) = do
--      maybe (return ()) (\x -> (liftIO.x) m) rH
      return ()

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
dump s hdr bs = return ()
--dump s hdr bs = do
--  let tp = show (ofp_hdr_type hdr)
--  putStr $ printf "%-4s %-24s %s\n" s tp (hexdumpBs 32 " " "" (BS.take 32 bs))

defActions = [ OFPAT_OUTPUT,OFPAT_SET_VLAN_VID,OFPAT_SET_VLAN_PCP
             , OFPAT_STRIP_VLAN,OFPAT_SET_DL_SRC,OFPAT_SET_DL_DST
             , OFPAT_SET_NW_SRC,OFPAT_SET_NW_DST,OFPAT_SET_NW_TOS
             , OFPAT_SET_TP_SRC,OFPAT_SET_TP_DST
             ]

