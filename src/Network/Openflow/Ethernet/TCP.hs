{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, FlexibleInstances, DeriveDataTypeable #-}
module Network.Openflow.Ethernet.TCP (TCPFlag(..), TCP(..), putTCP, tcpFlagsOf) where

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
import qualified Data.BitSet.Generic as BB

-- import Debug.Trace
-- import Text.Printf

type TCPPort = Word16

data TCPFlag = FIN | SYN | RST | PSH | ACK | URG | ECE | CWR deriving (Eq, Ord, Show, Read)

-- FIXME: Enum range overflow
instance Enum TCPFlag where
  fromEnum FIN = 0x01
  fromEnum SYN = 0x02
  fromEnum RST = 0x04
  fromEnum PSH = 0x08
  fromEnum ACK = 0x10
  fromEnum URG = 0x20
  fromEnum ECE = 0x40
  fromEnum CWR = 0x80
  toEnum 0x01 = FIN
  toEnum 0x02 = SYN
  toEnum 0x04 = RST
  toEnum 0x08 = PSH
  toEnum 0x10 = ACK
  toEnum 0x20 = URG
  toEnum 0x40 = ECE
  toEnum 0x80 = CWR

--instance Enum [TCPFlag] where
--        fromEnum fs = foldl' (+) 0 $ map (bit . fromEnum) fs
--        toEnum i = map (toEnum . snd) . filter fst . flip zip [0..7] . map (testBit i) $ [0..7]

class TCP a where
  tcpSrcAddr    :: a -> IPv4Addr
  tcpDstAddr    :: a -> IPv4Addr
  tcpProto      :: a -> Word8
  tcpSrcPort    :: a -> TCPPort
  tcpDstPort    :: a -> TCPPort
  tcpSeqNo      :: a -> Word32
  tcpAckNo      :: a -> Word32
  tcpFlags      :: a -> Word8
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
           undelay dataoff . (\x -> (((x `div` 4) .&. 0xF) `shiftL` 4)) . fromIntegral =<< distance start
           tcpPutPayload x
           hlen' <- distance start
           message_end <- marker    {- 0  -}
           putIP (tcpSrcAddr x)     {- 4  -}
           putIP (tcpDstAddr x)     {- 8  -}
           putWord8 0               {- 9  -}
           putWord8 (tcpProto x)    {- 10 -}
           putWord16be (fromIntegral hlen')
           let crc = 0 `icsum16'` (unsafePerformIO $ BS.unsafePackAddressLen 10 (toAddr message_end))
                       `icsum16'` (unsafePerformIO $ BS.unsafePackAddressLen hlen' (toAddr start))
           undelay acrc (Word16be (fin_icsum16' crc))
           shrink message_end
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
    flags   = tcpFlags x
    wss     = tcpWinSize x
    isUrgent = ( flags .&. (fromIntegral $ fromEnum URG) ) /= 0
{-# INLINE putTCP #-}

tcpFlagsOf :: [TCPFlag] -> Word8
tcpFlagsOf = BB.toBits . BB.fromList

