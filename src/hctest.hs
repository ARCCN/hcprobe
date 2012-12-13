{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Messages
import HCProbe.FakeSwitch

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

import Control.Monad.STM
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

--TODO: rename to hcprobe ?

hexdumpBs :: Int -> String -> String -> BS.ByteString -> String
hexdumpBs n ds ts bs = concat $ concat rows
  where hexes :: [String]
        hexes = reverse $ BS.foldl (\acc w -> (printf "%02X" w :: String) : acc) [] bs
        rows  = unfoldr chunk hexes
        chunk [] = Nothing
        chunk xs = Just (intersperse ds (take n xs) ++ [ts], drop n xs)

encodePutM = bsStrict . runPut

encodeMsg = encodePutM . putMessage

type SwitchConfig = TVar OfpSwitchConfig

ofpClient sw host port = do
  switchCfg <- newTVarIO defaultSwitchConfig
  runTCPClient (clientSettings port host) (client sw switchCfg)

client :: OfpSwitchFeatures -> SwitchConfig -> Application IO
client sw cfg ad = appSource ad $$ conduit
  where

    conduit = do
      bs' <- await
      when (isJust bs') $ do
        let bs = fromJust bs'
        case (ofpParsePacket bs) of
          Just (msg, rest) -> (liftIO $ dump "IN:" (ofp_header msg) bs) >> dispatch msg >> leftover rest
          Nothing          -> return ()
      conduit

    dispatch msg@(OfpMessage hdr msgData) = case (parseMessageData msg) of
      Nothing   -> return ()
      Just msg' -> processMessage (ofp_hdr_type hdr) msg'

    processMessage OFPT_HELLO (OfpMessage hdr _) = sendReply (headReply hdr OFPT_HELLO) nothing

    processMessage OFPT_FEATURES_REQUEST (OfpMessage hdr msg) = sendReply reply nothing
      where reply = featuresReply openflow_1_0 sw (ofp_hdr_xid hdr)

    processMessage OFPT_ECHO_REQUEST (OfpMessage hdr (OfpEchoRequest payload)) = sendReply reply nothing
      where reply = echoReply openflow_1_0 payload (ofp_hdr_xid hdr)

    processMessage OFPT_SET_CONFIG (OfpMessage hdr (OfpSetConfig cfg')) = do
      liftIO $ atomically $ writeTVar cfg cfg'

    processMessage OFPT_GET_CONFIG_REQUEST (OfpMessage hdr msg) = do
      cfg' <- liftIO $ atomically $ readTVar cfg
      sendReply (getConfigReply hdr cfg') nothing

    processMessage OFPT_BARRIER_REQUEST msg = do
      -- TODO: do something, process all pkts, etc
      sendReply (headReply (ofp_header msg) OFPT_BARRIER_REPLY) nothing

    processMessage OFPT_VENDOR msg = do
      -- TODO: do something, process all pkts, etc
      let errT = OfpError (OFPET_BAD_REQUEST OFPBRC_BAD_VENDOR) (BS.empty)
      let reply = errorReply (ofp_header msg) errT
      sendReply (reply) nothing

    -- TODO: implement the following messages
    processMessage OFPT_PACKET_OUT (OfpMessage hdr msg) = nothing
    processMessage OFPT_FLOW_MOD (OfpMessage hdr msg) = nothing
    processMessage OFPT_STATS_REQUEST (OfpMessage hdr msg) = nothing

    processMessage _ _ = nothing

    nothing = return ()

    sendReply msg fm = fm >> do
      liftIO $ dump "OUT:" (ofp_header msg) replyBs
      lift $ yield replyBs $$ (appSink ad)
      where replyBs = encodeMsg msg

-- TODO: move liftIO here
-- TODO: truncate message by length in header
-- TODO: use logger / settings
dump :: String -> OfpHeader -> BS.ByteString -> IO ()
dump s hdr bs = do
  let tp = show (ofp_hdr_type hdr)
  putStr $ printf "%-4s %-24s %s\n" s tp (hexdumpBs 32 " " "" (BS.take 32 bs))


main :: IO ()
main = do
  (host:port:_) <- getArgs
  rnd <- newStdGen
--  let (p,g) = makePort (defaultPortGen rnd) [] [] [OFPPF_1GB_HD,OFPPF_COPPER]
--  let q = encodePutM (putOfpPort p)
--  printf "Port Len: %d\n" (BS.length q)
  let (sw,g') = makeSwitch (defaultSwGen rnd) 48 [] [] [] [OFPPF_1GB_FD,OFPPF_COPPER]
  let hdr = header openflow_1_0 1 OFPT_FEATURES_REPLY
  let feature_repl = OfpMessage hdr (OfpFeatureReply sw)
  let bs = bsStrict $ runPut (putMessage feature_repl)
  putStrLn (fmtSwitch sw)
  ofpClient sw (BS8.pack host) (read port)
  putStrLn "done"


