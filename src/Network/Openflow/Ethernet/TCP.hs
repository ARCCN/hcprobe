{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, DeriveDataTypeable #-}
module Network.Openflow.Ethernet.TCP (TCPFlag(..), TCP(..), putTCP) where

import Network.Openflow.Ethernet.Types
import Network.Openflow.Misc
import Control.Monad
-- import qualified Data.Set as S
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Word
import Network.Openflow.StrictPut
import Data.Bits
import Data.List (foldl')
import System.IO.Unsafe

-- import Debug.Trace
-- import Text.Printf

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
           start <- marker
  {- 2  -} putWord16be srcPort
  {- 4  -} putWord16be dstPort
  {- 8  -} putWord32be seqno
  {- 12 -} putWord32be ackno
  {- 13 -} dataoff <- delayedWord8  -- data offset
  {- 14 -} putWord8 flags
  {- 16 -} putWord16be wss
  {- 18 -} acrc <- delayedWord16be -- CRC
  {- 20 -} when isUrgent $ putWord16be (tcpUrgentPtr x)
           hlen <- distance start
           replicateM_ (hlen `mod` 4) (putWord8 0)
           undelay dataoff . fromIntegral =<< distance start
           tcpPutPayload x
           hlen' <- distance start
           let crc = 0 `icsum16'` (pseudoHdr hlen')
                       `icsum16'` (unsafePerformIO $ BS.unsafePackAddressLen hlen' (toAddr start))
           undelay acrc (fin_icsum16' crc)
  where
    pseudoHdr y = runPutToByteString 16 $ do
          putIP (tcpSrcAddr x)
          putIP (tcpDstAddr x)
          putWord8 0
          putWord8 (tcpProto x)
          putWord16be (fromIntegral y)
    srcPort = tcpSrcPort x
    dstPort = tcpDstPort x
    seqno   = tcpSeqNo x
    ackno   = tcpAckNo x
    -- dataoff = (((hlen `div` 4) .&. 0xF) `shiftL` 4)
    flags   = (fromIntegral $! fromEnum (tcpFlags x))
    wss     = tcpWinSize x
    isUrgent = ( flags .&. (fromIntegral $ fromEnum URG) ) /= 0
{-# INLINABLE putTCP #-}
