{-# Language BangPatterns, ScopedTypeVariables, CPP #-}
module HCProbe.FakeSwitch ( PortGen(..), FakeSwitch(..)
                          , makePort
                          , makeSwitch
                          , defaultPortGen
                          , defaultSwGen
                          , fmtMac, fmtPort, fmtSwitch
                          , mcPrefix
                          , ofpClient
                          , arpGrat
                          , defActions
                          , defaultPacketInPort
                          , encodeMsg
                          , maxBuffers
                          , dump
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
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import Control.Monad
import Control.Error
import Control.Monad.State
import Control.Monad.STM
import Control.Monad.Trans.Resource
import Data.Bits
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.TMChan
import Data.Conduit.BinaryParse
import qualified Data.Conduit.List as CL
import Data.List
import Data.Maybe
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as M 
import Network.Openflow.StrictPut
import System.Random
import Text.Printf
import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
#ifdef RUNTIMETESTS
import Data.ByteString.Lazy.Builder
#endif 

import Debug.Trace

ethernetFrameMaxSize = 2048

maxBuffers = 0xFFFFFFF0

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
        gen' = gen { pnum = succ pn, rndGen = (snd.head.reverse) mac' }
        mac' = take 3 $ unfoldr ( \g -> Just (rand g, snd (rand g)) ) (rndGen gen)
        macbytes =  [0x00, 0x16, 0x3e] ++ map fst mac' :: [Word8]
        fmac acc b = (acc `shiftL` 8) .|. (fromIntegral b :: Word64)
        rand :: StdGen -> (Word8, StdGen)
        rand = random
        port = OfpPhyPort { ofp_port_no         = fromIntegral pn
                          , ofp_port_hw_addr    = foldl fmac 0 macbytes
                          , ofp_port_name       = pnm
                          , ofp_port_config     = listToFlags ofConfigFlags cfg --S.fromList cfg
                          , ofp_port_state      = listToFlags ofStateFlags st   --S.fromList st
                          , ofp_port_current    = listToFlags ofFeatureFlags ft --S.fromList ft
                          , ofp_port_advertised = listToFlags ofFeatureFlags ft --S.fromList ft
                          , ofp_port_supported  = listToFlags ofFeatureFlags ft --S.fromList ft
                          , ofp_port_peer       = listToFlags ofFeatureFlags ft --S.fromList ft
                          }

data SwitchGen = SwitchGen {  dpid    :: Int
                           ,  ipAddr  :: IPv4Addr
                           ,  swRnd   :: StdGen
                           }
defaultSwGen :: Int -> IPv4Addr -> StdGen -> SwitchGen
defaultSwGen i ip g = SwitchGen i ip g

data FakeSwitch = FakeSwitch {  switchFeatures :: OfpSwitchFeatures
                              , switchIP       :: IPv4Addr
                              , macSpace       :: M.IntMap (V.Vector MACAddr)
                              , onSendMessage  :: Maybe (OfpMessage -> IO ())
                              , onRecvMessage  :: Maybe (OfpMessage -> IO ())
                             }

makeSwitch :: SwitchGen
           -> Int
           -> [MACAddr]
           -> [OfpCapabilities]
           -> [OfpActionType]
           -> [OfpPortConfigFlags]
           -> [OfpPortStateFlags]
           -> [OfpPortFeatureFlags]
           -> (FakeSwitch, SwitchGen)

mcPrefix :: MACAddr -> MACAddr
mcPrefix = ((.|.)(0x00163e `shiftL` 24)).((.&.)0xFFFFFF)

makeSwitch gen ports mpp cap act cfg st ff = (FakeSwitch features (ipAddr gen) ms Nothing Nothing, gen')
  where features = OfpSwitchFeatures { ofp_datapath_id  = fromIntegral (dpid gen)
                                     , ofp_n_buffers    = maxBuffers 
                                     , ofp_n_tables     = 1
                                     , ofp_capabilities = listToFlags ofCapabilities cap
                                     , ofp_actions      = listToFlags ofActionType act
                                     , ofp_ports        = pps
                                     }
        gen' = gen { dpid = succ (dpid gen), swRnd = rndGen pg' }
        (pps, pg') = flip runState pg $ replicateM ports genPort
        pg = defaultPortGen (swRnd gen)

        genPort = do
          g <- get
          let (p,g') = makePort g cfg st ff
          put g'
          return p

        ms = M.fromList $ zip [1..nport] (map V.fromList macll)

        macll = take nport $ unfoldr (Just.(splitAt nmacpp)) mpp
        
        nmacpp  = nmac `div` nport

        nmac  = length mpp

        nport = length pps

fmtMac :: MACAddr -> String
fmtMac mac = intercalate ":" $ map (printf "%02X") bytes
  where bytes = drop 2 $ unpack64 mac

fmtPort :: OfpPhyPort -> String
fmtPort p = printf "%02d %-6s HWAddr:%18s, ST: %s, FT: %s" pno pname mac st ft
  where pno = ofp_port_no p
        pname = BS8.unpack (ofp_port_name p)
        mac = fmtMac (ofp_port_hw_addr p)
        st  = show $ flagsToList ofStateUnflag (ofp_port_state p) --show $ S.toList (ofp_port_state p)
        ft  = show $ flagsToList ofFeatureUnflag (ofp_port_current p) --show $ S.toList (ofp_port_current p)

fmtSwitch :: OfpSwitchFeatures -> String
fmtSwitch f = printf "DPID: %s, %s\n" dp cap ++ intercalate "\n" ports
  where dp  = fmtMac (ofp_datapath_id f)
        cap = show $ flagsToList ofCapabilitiesUnflag (ofp_capabilities  f) --show (S.toList (ofp_capabilities f)) 
        ports = map fmtPort (ofp_ports f) 

encodeMsg = runPutToByteString 32768 . putMessage

data SwitchContext = SwitchContext { handshakeDone :: TVar Bool
                                   , transactionID :: TVar Int
                                   , switchCfg     :: TVar OfpSwitchConfig
                                   }

pktSendTimeout = 5000000
pktSendQLen    = 65536

sendLen = 65536 `div` 2

ofpClient pktGen sw host port = runTCPClient (clientSettings port host) (client pktGen sw)

client pktInGen fk@(FakeSwitch sw switchIP l_ sH rH) ad = runResourceT $ do
    (_, !pktSendQ) <- allocate (newTBMChanIO pktSendQLen) (atomically.closeTBMChan)
    buf <- liftIO $ mkBuffer (1024*1024*4)
    go pktSendQ buf
    where
        go pktSendQ buf = do
            tranId <- liftIO $ newTVarIO 0

            delayed <- liftIO $ newTVarIO ([], 0)

            featureReplyMonitor <- liftIO $ newTVarIO False
            swCfg <- liftIO $ newTVarIO defaultSwitchConfig

            let !ctx = SwitchContext featureReplyMonitor tranId swCfg

            let sender = sourceTBMChan pktSendQ 
                  $= CL.mapM stat
                  -- =$= CL.mapM (\x -> putStrLn "OUT:" >> putStrLn (show x) >> return x)
                  =$= CL.mapM (\x -> 
#ifdef RUNTIMETESTS
                       do let rx = BS.concat . LBS.toChunks . toLazyByteString $ buildMessage x
                          mx <- extract <$> runPutToBuffer buf (putMessage x)
                          unless (rx == mx) (do putStrLn $ printf "/!\\ ACHTUNG /!\\"
                                                putStrLn $ show rx
                                                putStrLn $ show mx
                                            )
                          return mx
#else
                       extract <$> runPutToBuffer buf (putMessage x)
#endif
                       )
                  $$ appSink ad
                stat   = maybe return (\x -> (\y -> liftIO (x y) >> return y)) sH
                  

            -- TODO: write ofp correct reader using conduit/other iterative
            -- interface
            let receiver = appSource ad -- $= CL.mapM (\x -> putStrLn "IN:" >> putStrLn (show (BS.unpack x)) >> return x)
                                        $= conduitBinary -- :: Conduit BS8.ByteString Undef OfpMessage)
                                        -- =$= CL.mapM (\x -> putStrLn (show x) >> return x)
                                        $$ CL.mapM_ (\m@(OfpMessage h _) ->
                                                    processMessage ctx (ofp_hdr_type h) m)

            let sendARPGrat = do
                withTimeout pktSendTimeout (readTVar featureReplyMonitor >>= flip unless retry)
                liftM (arpGrat fk (-1 :: Word32)) (nextTranID ctx) >>= sendReplyT

            let threads = [receiver, sender, 
                           do withTimeout pktSendTimeout (readTVar featureReplyMonitor >>= flip unless retry)
                              pktInGen fk pktSendQ]
            waitThreads <- liftIO $ mapM asyncBound threads
            mapM_ (flip allocate cancel) (map return waitThreads)
            liftIO $ do
              async sendARPGrat
              v <- waitAnyCatchCancel waitThreads
              case snd v of
                  Left e -> putStrLn (show e)
                  Right _ -> return ()
          where
            sendReplyT msg = do
              --liftIO $ dump "OUT:" (ofp_header msg) 
              atomically $ writeTBMChan pktSendQ msg
              maybe (return ()) (\x -> (liftIO.x) msg) sH

{-
            dispatch !c msg@(OfpMessage hdr msgData)) = case (parseMessageData msg) of
              Nothing   ->  return ()
              Just !(msg'@(OfpMessage h _)) -> processMessage c (ofp_hdr_type hdr) msg'
-}

            -- TODO: implement the following messages
            processMessage _ OFPT_PACKET_OUT m@(OfpMessage hdr msg) = do
              maybe (return ()) (\x -> (liftIO.x) m) rH

            processMessage _ OFPT_HELLO (OfpMessage hdr _) = sendReplyT (headReply hdr OFPT_HELLO)

            processMessage c OFPT_FEATURES_REQUEST (OfpMessage hdr msg) = sendReplyT reply
              where reply = featuresReply openflow_1_0 sw (ofp_hdr_xid hdr)

            processMessage _ OFPT_ECHO_REQUEST (OfpMessage hdr (OfpEchoRequest payload)) = sendReplyT reply
              where reply = echoReply openflow_1_0 payload (ofp_hdr_xid hdr)

            processMessage c OFPT_SET_CONFIG (OfpMessage hdr (OfpSetConfig cfg')) = do
              liftIO $ atomically $ modifyTVar (switchCfg c) (const cfg')

            processMessage c OFPT_GET_CONFIG_REQUEST (OfpMessage hdr msg) =
              (liftIO $ atomically $ readTVar (switchCfg c)) >>= \x -> sendReplyT (getConfigReply hdr x)

            processMessage c OFPT_STATS_REQUEST (OfpMessage hdr (OfpStatsRequest OFPST_DESC)) = sendReplyT (statsReply hdr)
        --      (liftIO $ atomically $ readTVar (switchCfg c)) >>= sendReply.getConfigReply hdr

            -- FIXME: possible problems with other controllers rather than NOX
            processMessage c OFPT_BARRIER_REQUEST msg = do
              -- TODO: do something, process all pkts, etc
              sendReplyT (headReply (ofp_header msg) OFPT_BARRIER_REPLY)
              liftIO $ atomically (writeTVar (handshakeDone c) True)

            processMessage _ OFPT_VENDOR msg = do
              let errT = OfpError (OFPET_BAD_REQUEST OFPBRC_BAD_VENDOR) BS.empty
              let reply = errorReply (ofp_header msg) errT
              sendReplyT reply

            -- TODO: implement the following messages
            processMessage _ OFPT_FLOW_MOD (OfpMessage hdr msg) = nothing
            processMessage _ OFPT_STATS_REQUEST (OfpMessage hdr msg) = nothing

            processMessage _ _ _ = nothing

            nothing = return ()

            nextTranID c = liftIO $ atomically $ do
              modifyTVar (transactionID c) succ
              readTVar (transactionID c) >>= return . fromIntegral

            withTimeout :: Int -> STM a -> IO (Maybe a)
            withTimeout tv f = do
              x <- registerDelay tv
              atomically $! (readTVar x >>= \y -> if y
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
        arpGratData = putEthernetFrame (ARPGratuitousReply mac ip)
        mac = ofp_datapath_id sw
        ip  = switchIP fk 
        sw  = switchFeatures fk


-- TODO: move liftIO here
-- TODO: truncate message by length in header
-- TODO: use logger / settings
dump :: String -> OfpHeader -> BS.ByteString -> IO ()
dump _s _hdr _bs = return ()
--dump s hdr bs = do
--  let tp = show (ofp_hdr_type hdr)
--  putStr $ printf "%-4s %-24s %s\n" s tp (hexdumpBs 32 " " "" (BS.take 32 bs))

defActions = [ OFPAT_OUTPUT,OFPAT_SET_VLAN_VID,OFPAT_SET_VLAN_PCP
             , OFPAT_STRIP_VLAN,OFPAT_SET_DL_SRC,OFPAT_SET_DL_DST
             , OFPAT_SET_NW_SRC,OFPAT_SET_NW_DST,OFPAT_SET_NW_TOS
             , OFPAT_SET_TP_SRC,OFPAT_SET_TP_DST
             ]

