{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DeriveDataTypeable #-}
module Network.Openflow.Ethernet.TCP (TCPFlag(..), TCP(..), putTCP) where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Control.Monad
import qualified Data.Set as S
import qualified Data.ByteString as BS
import Data.Word
import Nettle.OpenFlow.StrictPut 
import Data.Bits
import Data.List (foldl')

import Debug.Trace
import Data.Maybe
import Text.Printf

type TCPPort = Word16

data TCPFlag = FIN | SYN | RST | PSH | ACK | URG | ECE | CWR deriving (Eq, Ord, Show, Read, Enum)

instance Enum [TCPFlag] where
        fromEnum fs = foldl' (+) 0 $ map (bit . fromEnum) fs
        toEnum i = map (toEnum . snd) . filter fst . flip zip [0..7] . map (testBit i) $ [0..7]

class TCP a where
  tcpSrcAddr    :: a -> IPv4Addr
  tcpDstAddr    :: a -> IPv4Addr
  tcpProto      :: a -> Word8
  tcpSrcPort    :: a -> TCPPort
  tcpDstPort    :: a -> TCPPort
  tcpSeqNo      :: a -> Word32
  tcpAckNo      :: a -> Word32
  tcpFlags      :: a -> [TCPFlag]
  tcpWinSize    :: a -> Word16
  tcpUrgentPtr  :: a -> Word16
  tcpPutPayload :: a -> PutM ()


-- TODO: header generation may be improved
-- TODO: checksum generation may be improved

putTCP :: TCP a => a -> PutM ()
putTCP x = do
--  trace ( (printf "%04X" (fromJust $ csum16 pkt))) $ return ()
--  trace ( (hexdumpBs 160 " " "" pkt) ++ "\n") $ return ()
  putHeader (csum16 pkt) >> putByteString body

  where putHeader cs = do
  {- 2  -} putWord16be srcPort
  {- 4  -} putWord16be dstPort
  {- 8  -} putWord32be seqno
  {- 12 -} putWord32be ackno
  {- 13 -} putWord8    (fromIntegral dataoff)
  {- 14 -} putWord8    flags
  {- 16 -} putWord16be wss
  {- 18 -} putWord16be (maybe 0 id cs)
  {- 20 -} when isUrgent $ putWord16be (tcpUrgentPtr x)
  {- ?? -} padding

        padding = replicateM_ ( hlen' `mod` 4 ) (putWord8 0)

        pseudoHdr = runPutToByteString 256 $ do
          putIP (tcpSrcAddr x)
          putIP (tcpDstAddr x)
          putWord8 0
          putWord8 (tcpProto x)
          putWord16be (fromIntegral $ (fromIntegral hlen) + BS.length body)

        hdr =  runPutToByteString 128  (putHeader Nothing)
        body = runPutToByteString 2048 (tcpPutPayload x)
        pkt = BS.concat [pseudoHdr, hdr, body]
      
        hlen =  hlen' + hlen' `mod` 4

        hlen' | isUrgent  = 20 
              | otherwise = 18

        srcPort = tcpSrcPort x
        dstPort = tcpDstPort x
        seqno   = tcpSeqNo x
        ackno   = tcpAckNo x
        dataoff = (((hlen `div` 4) .&. 0xF) `shiftL` 4)
        flags   = (fromIntegral $ fromEnum (tcpFlags x))
        wss     = tcpWinSize x
        isUrgent = ( flags .&. (fromIntegral $ fromEnum URG) ) /= 0

        csum16 = const $ Just 0xFF 


