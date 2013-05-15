-- | Library for processing messages
module HCProbe.FakeSwitch.Processing
  ( defProcessMessage
  , printType    -- FIXME rename to dumpType
  , printMessage -- FIXME rename to dumpMessage
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Control.Concurrent.STM

import qualified Data.ByteString as BS

import Data.Conduit
import Data.Conduit.List as CL
import Network.Openflow.Types
import Network.Openflow.Messages
import HCProbe.FakeSwitch

printType ::(MonadIO m) => Conduit (OfpType, OfpMessage) m (OfpType, OfpMessage)
printType = CL.mapM (\x -> liftIO (print (fst x)) >> return x)

printMessage ::(MonadIO m) => Conduit (OfpType, OfpMessage) m (OfpType, OfpMessage)
printMessage = CL.mapM (\x -> liftIO (print (snd x)) >> return x)

defProcessMessage _ _  OFPT_HELLO (OfpMessage hdr _) = nothing -- return $ Just (headReply hdr OFPT_HELLO)
defProcessMessage fk swCfg OFPT_FEATURES_REQUEST (OfpMessage hdr _msg) = 
          return $ Just (featuresReply openflow_1_0 (eSwitchFeatures fk) (ofp_hdr_xid hdr))
defProcessMessage _ _ OFPT_ECHO_REQUEST (OfpMessage hdr (OfpEchoRequest payload)) = return $ Just reply
          where reply = echoReply openflow_1_0 payload (ofp_hdr_xid hdr)
defProcessMessage _ swCfg OFPT_SET_CONFIG (OfpMessage _ (OfpSetConfig cfg')) = do
          liftIO $ atomically $ modifyTVar swCfg (const cfg')
          return Nothing
defProcessMessage _ swCfg OFPT_GET_CONFIG_REQUEST (OfpMessage hdr _msg) = do
          x <- readTVarIO swCfg
          return (Just (getConfigReply hdr x))
defProcessMessage _ _ OFPT_STATS_REQUEST (OfpMessage hdr (OfpStatsRequest OFPST_DESC)) = 
          return $ Just (statsReply hdr)
defProcessMessage _ _ OFPT_BARRIER_REQUEST msg = do
          liftIO $ putStrLn "wtf?!"
          return $ Just (headReply (ofp_header msg) OFPT_BARRIER_REPLY)
defProcessMessage _ _ OFPT_VENDOR msg = 
          let errT = OfpError (OFPET_BAD_REQUEST OFPBRC_BAD_VENDOR) BS.empty
              reply = errorReply (ofp_header msg) errT
          in return $ Just reply
-- defProcessMessage OFPT_FLOW_MOD (OfpMessage hdr msg) = return Nothing
-- defProcessMessage OFPT_STATS_REQUEST (OfpMessage hdr msg) = return Nothing
-- defProcessMessage OFPT_PACKET_OUT m@(OfpMessage hdr msg) = return $ Nothing
defProcessMessage _ _ _ _ = nothing 

nothing = return Nothing

