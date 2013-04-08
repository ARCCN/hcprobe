{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |  This module is improved version of Nettle.OpenFlow.StrictPut 
--    I hope we'll merge changes someday.
--
-- This module provides a monad for serializing data into byte strings.
-- It provides mostly the same interface that Data.Binary.Put does.
-- However, the implementation is different. It allows for the data to be
-- serialized into an existing array of Word8 values. This differs from the Data.Binary.Put
-- data type, which allocates a Word8 array every time a value is serialized.
-- This module's implementation is useful if you want to reuse the Word8 array for many serializations.
-- In the case of an OpenFlow server, we can reuse a buffer to send messages, since we have no use
-- for the the Word8 array, except to pass it to an IO procedure to write the data to a socket or file.
module Network.Openflow.StrictPut (
  PutM,
  Put,
  runPut,
  runPutToByteString,
  putWord8,
  putWord16be,
  putWord32be,
  putWord64be,
  putByteString,
  -- * Markers
  Marker,
  marker,
  toAddr,
  distance,
  shrink,
  -- * Delay
  DelayedPut,
  undelay,
  -- contramap,
  delayedWord8,
  delayedWord16be,
  Word16be(..),
  RPut(..),
  -- * Buffer
  runPutToBuffer,
  Buffer,
  mkBuffer,
  extract,
  bufferSize,
  reuse
  ) where

import Control.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import GHC.Word
import Foreign
import GHC.Exts
import System.IO.Unsafe

-- A state monad with state being the pointer to write location.
newtype PutM a = PutM { unPut :: Ptr Word8 -> IO (a, Ptr Word8) }

type Put = PutM ()

-- | Runs the Put writer with write position given
-- by the first pointer argument. Returns the number
-- of words written.
runPut :: Ptr Word8 -> Put -> IO Int
runPut ptr (PutM f) =
     do (_, ptr') <- f ptr
        return (ptr' `minusPtr` ptr)

-- | Allocates a new byte string, and runs the Put writer with that byte string.
-- The first argument is an upper bound on the size of the array needed to do the serialization.
runPutToByteString :: Int -> Put -> S.ByteString
runPutToByteString maxSize put =
  unsafeDupablePerformIO (S.createAndTrim maxSize (\ptr -> runPut ptr put))
  -- unsafeDupablePerformIO (S.createAndTrim maxSize (\ptr -> (`minusPtr` ptr) <$> runPut ptr put ))
  
instance Monad PutM where
  return x = PutM (\ptr -> return (x, ptr))
  {-# INLINE return #-}
  (PutM m) >>= f = PutM (\(!ptr) -> do
      (a, ptr') <- m ptr
      case f a of 
        PutM !g -> g ptr')
  {-# INLINE (>>=) #-}
  
putWord8 :: Word8 -> Put
putWord8 !w = PutM (\(!ptr) -> do { poke ptr w; return ((), ptr `plusPtr` 1) })
{-# INLINE putWord8 #-}

putWord16be :: Word16 -> Put
putWord16be !w = PutM f
  where f !ptr =
          do poke ptr (fromIntegral (shiftr_w16 w 8) :: Word8)
             poke (ptr `plusPtr` 1) (fromIntegral (w) :: Word8)
             return ((), ptr `plusPtr` 2)
{-# INLINE putWord16be #-}

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> Put
putWord32be !w = PutM f
  where f !p =
          do poke p (fromIntegral (shiftr_w32 w 24) :: Word8)
             poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
             poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 8) :: Word8)
             poke (p `plusPtr` 3) (fromIntegral (w) :: Word8)
             return ((), p `plusPtr` 4)
{-# INLINE putWord32be #-}

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> Put
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
putWord64be !w =
    let a = fromIntegral (shiftr_w64 w 32) :: Word32
        b = fromIntegral w :: Word32
    in PutM $ \(!p) -> do
      poke p (fromIntegral (shiftr_w32 a 24) :: Word8)
      poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
      poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 8) :: Word8)
      poke (p `plusPtr` 3) (fromIntegral (a) :: Word8)
      poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
      poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
      poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 8) :: Word8)
      poke (p `plusPtr` 7) (fromIntegral (b) :: Word8)
      return ((), p `plusPtr` 8)
#else
putWord64be !w = PutM $ \(!p) -> do
  poke p (fromIntegral (shiftr_w64 w 56) :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
  poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
  poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
  poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
  poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
  poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 8) :: Word8)
  poke (p `plusPtr` 7) (fromIntegral (w) :: Word8)
  return ((), p `plusPtr` 8)
#endif
{-# INLINE putWord64be #-}

putByteString :: S.ByteString -> Put
putByteString !bs = PutM f
  where f !ptr =
          let (fp, offset, len) = S.toForeignPtr bs
          in do withForeignPtr fp $ \bsptr -> S.memcpy ptr (bsptr `plusPtr` offset) (fromIntegral len)
                return ((), ptr `plusPtr` len)
{-# INLINE putByteString #-}

-- | get current address

-- | Mark currect address
newtype Marker = Marker (Ptr Word8)

-- | Create new marker at current position/
marker :: PutM Marker
marker = PutM $ \x -> return (Marker x, x)
{-# INLINE marker #-}

-- | Find difference in current position and marker.
distance :: Marker 
         -> PutM Int
distance (Marker x) = PutM $ \x' -> return (x' `minusPtr` x, x')
{-# INLINE distance #-}

shrink :: Marker 
       -> Put
shrink (Marker x) = PutM $ \_ -> return ((),x)

-- | Get real address
toAddr :: Marker -> Addr#
toAddr (Marker (Ptr a)) = a
{-# INLINE toAddr #-}

class RPut a where
  rput :: Ptr a -> a -> IO ()

instance RPut Word8 where
  rput = poke
  {-# INLINE rput #-}

newtype Word16be = Word16be Word16 deriving (Num)

instance RPut Word16be where
  rput p (Word16be x) = void (runPut (castPtr p) (putWord16be x))
  {-# INLINE rput #-}

-- | Delayed action.
newtype DelayedPut a = DelayedPut (Ptr a)

--contramap :: (a -> b) -> (DelayedPut b) -> (DelayedPut a)
--contramap f (DelayedPut g) = DelayedPut (g.f)

undelay :: (RPut a)
        => DelayedPut a 
        -> a 
        -> PutM ()
undelay (DelayedPut a) !x = PutM $ \p -> rput a x >> return ((),p)
{-# INLINE undelay #-}

delayedWord8 :: PutM (DelayedPut Word8)
delayedWord8 = PutM $ \p -> poke p (0::Word8) >> 
                            return (DelayedPut p, p `plusPtr` 1)
{-# INLINE delayedWord8 #-}

delayedWord16be :: PutM (DelayedPut Word16be)
delayedWord16be = PutM $ \p -> poke (castPtr p) (0::Word16) >>
                               return (DelayedPut (castPtr p), p `plusPtr` 2)
{-# INLINE delayedWord16be #-}

{-# INLINE shiftr_w16 #-}
shiftr_w16 :: Word16 -> Int -> Word16
{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
{-# INLINE shiftr_w64 #-}
shiftr_w64 :: Word64 -> Int -> Word64


#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#` i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#` i)

#if WORD_SIZE_IN_BITS < 64
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)
#endif
#endif


data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)   -- pinned array
                     {-# UNPACK #-} !(Ptr Word8)          -- current position
                     {-# UNPACK #-} !Int                  -- current size

bufferSize :: Buffer -> Int
bufferSize (Buffer _ _ i) = i

reuse :: Buffer -> Buffer
reuse (Buffer f _ _) = Buffer f (unsafeForeignPtrToPtr f) 0

extract :: Buffer -> S.ByteString
extract (Buffer f _ c) = S.fromForeignPtr f 0 c

mkBuffer :: Int -> IO Buffer
mkBuffer size = do
    fpbuf <- S.mallocByteString size
    let !pbuf = unsafeForeignPtrToPtr fpbuf
    return $! Buffer fpbuf pbuf 0 


runPutToBuffer :: Buffer -> Put -> IO Buffer
runPutToBuffer (Buffer f p x) put = runPut p put >>= \i -> return (Buffer f (p `plusPtr`i)  (x+i))
