{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DeriveDataTypeable #-}
module Network.Openflow.Ethernet.TCP (TCPFlag) where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Control.Monad
import qualified Data.Set as S
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary.Put
import Data.Bits
import Data.List (foldl')

type TCPPort = Word16

data TCPFlag = FIN | SYN | RST | PSH | ACK | URG | ECE | CWR deriving (Eq, Ord, Show, Read, Enum)

instance Enum [TCPFlag] where
        fromEnum fs = foldl' (+) 0 $ map (bit . fromEnum) fs
        toEnum i = map (toEnum . snd) . filter fst . flip zip [0..7] . map (testBit i) $ [0..7]

class TCP a where
  tcpSrcPort    :: a -> TCPPort
  tcpDstPort    :: a -> TCPPort
  tcpSeqNo      :: a -> Word32
  tcpAckNo      :: a -> Word32
  tcpFlags      :: a -> [TCPFlag]
  tcpWinSize    :: a -> Word16
  tcpUrgentPtr  :: a -> Word16
  tcpPutPayload :: a -> PutM ()

putTCP :: TCP a => a -> PutM ()
putTCP x = putHeader (csum16 pkt) >> (tcpPutPayload x)

  where putHeader cs = do
  {- 2  -} putWord16be srcPort
  {- 4  -} putWord16be dstPort
  {- 8  -} putWord32be seqno
  {- 12 -} putWord32be ackno
  {- 13 -} putWord8    (fromIntegral hlen)
  {- 14 -} putWord8    flags
  {- 16 -} putWord16be wss
  {- 20 -} putWord16be (maybe 0 id cs)
  {- 22 -} when isUrgent $ putWord16be (tcpUrgentPtr x)
  {- ?? -} padding

        padding = replicateM_ ( hlen' `mod` 4 ) (putWord8 0)

        hdr = (bsStrict . runPut) (putHeader Nothing)
        body = (bsStrict . runPut) (tcpPutPayload x)
        pkt = BS.concat [hdr, body]
      
        hlen =  hlen' + hlen' `mod` 4

        hlen' | isUrgent  = 22
              | otherwise = 20

        srcPort = tcpSrcPort x
        dstPort = tcpDstPort x
        seqno   = tcpSeqNo x
        ackno   = tcpAckNo x
        dataoff = ((hlen .&. 0xF) `shiftL` 4)
        flags   = (fromIntegral $ fromEnum (tcpFlags x))
        wss     = tcpWinSize x
        isUrgent = ( flags .|. (fromIntegral $ fromEnum URG) ) /= 0


