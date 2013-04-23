{-# LANGUAGE ForeignFunctionInterface, BangPatterns, MagicHash #-}

module Main ( main
            ) where

import Data.Word
import Data.Bits
import Control.Applicative
import qualified Data.Vector.Storable as V
import qualified Data.Binary.Put as BP 
import Data.Binary.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Control.DeepSeq

import Foreign.ForeignPtr
import GHC.Ptr
import Foreign.C
import qualified Foreign.Ptr as FP

import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import System.Mem

import Criterion
import Criterion.Main

import Network.Openflow.Misc

bsFromW16' w16 = BP.runPut $ BP.putBuilder $ foldl accPutWord16be BB.empty w16
    where accPutWord16be acc w = acc `BB.append` (BB.putWord16be w)

bsFromW16 wlist16 = BS.pack $ wlist8 wlist16
wlist8 wlist16 = foldr w16towlist8 [] wlist16
    where w16towlist8 :: Word16 -> [Word8] -> [Word8]
          w16towlist8 w16 acc = fromIntegral ( w16 `shift` (-8) )
                              : fromIntegral w16
                              : acc
testPrepareOld wlist8 =
    BS.pack (wlist8 ++ wlist8)
testPrepareNew wlist8 =
    V.unsafeCast $ V.fromList (wlist8 ++ wlist8)

rtest16  start len = 
        (V.unsafeCast (V.unsafeFromForeignPtr0 
        (BS.inlinePerformIO (newForeignPtr_ start)) len)) -- FIXME:Word8 to Word16 here the problem 
rtest16' (Ptr start) len = (BS.inlinePerformIO $ BS.unsafePackAddressLen len start)

testCompare wlist8 =
    (icsum16 0 $ testPrepareNew wlist8 ) == (icsum16' 0 $ testPrepareOld wlist8)

foreign import ccall unsafe "icsum.h c_icsum16" c_icsum16 :: CUInt -> FP.Ptr () -> CSize -> CUInt 

instance NFData CUInt where
    rnf = rnf . (fromIntegral :: CUInt -> Word32)

toCIcsum16 = c_icsum16 0

genLengthDEF :: Int
genLengthDEF = 1024
genAmountDEF :: Int
genAmountDEF = 500

main = do
    gen <- newStdGen
    let test = unGen (vector genLengthDEF) gen genAmountDEF :: [[Word8]]

    quickCheck (\x -> icsum16 0 (testPrepareNew x) == icsum16' 0 (testPrepareOld x))
    quickCheck (\x -> 
                    let (mem,_ofs,len) = BS.toForeignPtr (BS.pack $ x++x)
                        !mem' = unsafeForeignPtrToPtr mem -- for convinience
                        !(Ptr a') = mem'
                        !len' = (len `div` 2)
                    in fromIntegral (toCIcsum16 (FP.castPtr mem') (fromIntegral len')) == icsum16' 0 (testPrepareOld x))
    quickCheck (\x -> 
                    let (mem,_ofs,len) = BS.toForeignPtr (BS.pack (x++x))
                        !mem' = unsafeForeignPtrToPtr mem -- for convinience
                        !(Ptr a#) = mem'
                        a = (icsum16p 0 a# len) 
                        b = icsum16' 0 (testPrepareOld x)
                    in a == b)

    (mem,_ofs,len) <- BS.toForeignPtr . BS.pack . head <$> sample' (vector genLengthDEF)
    let !mem' = unsafeForeignPtrToPtr mem -- for convinience
    let !(Ptr a') = mem'
    let !len' = (len `div` 2)

    performGC
    defaultMain [ bcompare
                        [ bench "csumC" $ nf (uncurry toCIcsum16) 
                            (FP.castPtr mem', fromIntegral len')
                        , bench "csumNew"  $ nf (icsum16 0) (rtest16 mem' len)
                        , bench "csumNewP" $ nf (icsum16p 0 a') len
                        , bench "csumOld"  $ nf (icsum16' 0) (rtest16' mem' len)
                        ]
                , bgroup "from list"     
                    [  bench "csumNew" $ nf (map (icsum16 0)) (map testPrepareNew test)
                    ,  bench "csumOld" $ nf (map (icsum16' 0)) (map testPrepareOld test)
                    ]
                ]
    touchForeignPtr mem -- to sade data from gc destructor
