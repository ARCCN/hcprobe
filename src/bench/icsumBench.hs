{-# LANGUAGE ForeignFunctionInterface #-}
{-# INCLUDE "icsum.h" #-}

module Main ( main
            ) where

import System.Environment
import Network.Openflow.Misc

import Network.Openflow.Types
import Network.Openflow.Ethernet.Types
import Data.Word
import Data.Bits
import Data.Monoid
import qualified Data.Vector.Storable as V
import Control.Applicative
import Network.Openflow.StrictPut
import qualified Data.Binary.Put as BP 
import Data.Binary.Get
import Data.Binary.Builder as BB
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BS
import Control.Monad
import Control.DeepSeq

import Foreign.Storable
import Foreign.ForeignPtr
import GHC.Ptr
import Foreign.C
import qualified Foreign.Ptr as FP

import Text.Printf
import Debug.Trace

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Random

import Criterion
import Criterion.Main

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

rtest16  start len = icsum16 0 (V.unsafeCast (V.unsafeFromForeignPtr0 
        (BS.inlinePerformIO (newForeignPtr_ start)) len)) -- FIXME:Word8 to Word16 here the problem 
rtest16' (Ptr start) len = icsum16' 0 (BS.inlinePerformIO $ BS.unsafePackAddressLen len start)

testCompare wlist8 =
    (icsum16 0 $ testPrepareNew wlist8 ) == (icsum16' 0 $ testPrepareOld wlist8)

foreign import ccall "icsum.h c_icsum16" c_icsum16 :: CUInt -> FP.Ptr () -> CSize -> CUInt 

instance NFData CUInt where
    rnf = rnf . (fromIntegral :: CUInt -> Word32)

toCIcsum16 = c_icsum16 0

genLengthDEF = 100000 :: Int
genAmountDEF = 5 :: Int

main = do
    gen <- newStdGen
    let test = unGen (vector genLengthDEF) gen genAmountDEF :: [[Word8]]

    (mem,_ofs,len) <- BS.toForeignPtr . BS.pack . head <$> sample' (vector genLengthDEF)
    let mem' = unsafeForeignPtrToPtr mem -- for convinience
    let len' = len `div` 2

    defaultMain [ bgroup "from list"     
                    [ bench "csumOld" $ nf (map (icsum16' 0)) (map testPrepareOld test)
                    , bench "csumNew" $ nf (map (icsum16 0)) (map testPrepareNew test)
                    ]
                , bgroup "from memory"
                    [ bench "csumNew" $ nf (uncurry rtest16) (mem',len)
                    , bench "csumOld" $ nf (uncurry rtest16') (mem',len)
                    , bench "csumC" $ nf (uncurry toCIcsum16) 
                        (FP.castPtr mem', fromIntegral len')
                    ]
                ]
    touchForeignPtr mem -- to sade data from gc destructor
