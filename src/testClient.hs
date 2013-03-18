module Main ( main
            ) where

import System.Environment
import qualified HCProbe.FakeSwitch as FS
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
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as M 
import System.Random
import Text.Printf
import Data.Time

import Debug.Trace

fsDefaultHost       = "localhost"
fsDefaultPort       = "6633"

fsMacsPerPort       = 1000
fsPorts             = 48

pktSendTimeout      = 1
pktSendNumber       = 1000000

pktMsgOfVersion     = 1 :: Word8
pktMsgOfSwitch      = 1 :: Word32
pktMsgCapabuilities = [ OFPC_FLOW_STATS
                      , OFPC_TABLE_STATS
                      , OFPC_PORT_STATS
                      , OFPC_STP
                      , OFPC_RESERVED
                      , OFPC_IP_REASM
                      , OFPC_QUEUE_STATS
                      , OFPC_ARP_MATCH_IP
                      ]
pktMsgActionType    = FS.defActions
                      --[ OFPAT_OUTPUT
                      --, OFPAT_SET_VLAN_VID
                      --, OFPAT_SET_VLAN_PCP
                      --, OFPAT_STRIP_VLAN
                      --, OFPAT_SET_DL_SRC
                      --, OFPAT_SET_DL_DST
                      --, OFPAT_SET_NW_SRC
                      --, OFPAT_SET_NW_DST
                      --, OFPAT_SET_NW_TOS
                      --, OFPAT_SET_TP_SRC
                      --, OFPAT_SET_TP_DST
                      --, OFPAT_ENQUEUE
                      --, OFPAT_VENDOR
                      --]
pktMsgOfpPhyPorts   = []

pktSendMsg :: OfpMessage
pktSendMsg = OfpMessage hd dt
    where
        hd = header pktMsgOfVersion pktMsgOfSwitch OFPT_FEATURES_REPLY
        dt = OfpFeatureReply $ OfpSwitchFeatures 1000 100 1 cb at pktMsgOfpPhyPorts
        cb = S.fromList pktMsgCapabuilities
        at = S.fromList pktMsgActionType 

ofClient fk@(FS.FakeSwitch sw switchIP _ sH rH) ad = replicateM_ pktSendNumber $ do
        sendReplyT pktSendMsg
        M.threadDelay pktSendTimeout

    where
        sendReplyT msg = do
        liftIO $ FS.dump "OUT:" (ofp_header msg) replyBs
        --print $ BS8.length replyBs
        yield replyBs $$ (appSink ad)
        maybe (return ()) (\x -> (liftIO.x) msg) sH
        where replyBs = FS.encodeMsg msg

zeroClient ad = replicateM_ pktSendNumber $ do
    yield replyBs $$ (appSink ad)
    where 
        replyBs = BS.replicate 32 $ 0

ofpClient sw host port client = runTCPClient (clientSettings port host) client

main = do
    args <- getArgs
    let (host,port) = host_port args
    fakeSw <- do
        let i = 100
        let ip = fromIntegral i .|. (0x10 `shiftL` 24)
        rnd <- newStdGen
        let macs = map (\x->FS.mcPrefix x) [1..fsPorts]
        return $ fst $ FS.makeSwitch (FS.defaultSwGen i ip rnd) 
                        (fromIntegral fsPorts) 
                        macs [] FS.defActions [] [] [OFPPF_1GB_FD,OFPPF_COPPER]
    let client = take_client fakeSw args
    initTime <- getCurrentTime
    ofpClient fakeSw (BS8.pack host) (read port) client
    now <- getCurrentTime

    putStr "Time for all is: "
    let timeDif = now `diffUTCTime` initTime
    print timeDif
    putStr "Time per package: "
    print $ timeDif / (fromIntegral pktSendNumber)
    where
        host_port (host:port:_) = (host,port)
        host_port (host:[]) = (host, fsDefaultPort)
        host_port [] = (fsDefaultHost, fsDefaultPort)
        take_client _ (_:_:"-0":_) = zeroClient
        take_client fs _ = ofClient fs
