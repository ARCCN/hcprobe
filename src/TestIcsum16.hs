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
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import Control.Monad

import Foreign.Storable
import Foreign.ForeignPtr

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
testCompare wlist16 =
    (icsum16 0 $ V.fromList wlist16 ) == (icsum16' 0 $ bsFromW16 wlist16)

genLengthDEF = 100 :: Int

main = do
    test <- sample' arbitrary :: IO [[Word16]]

    defaultMain [ bench "csumNew" $ nf (map (icsum16 0)) (map V.fromList test)
                , bench "csumOld" $ nf (map (icsum16' 0)) (map bsFromW16 test)
                --, bench "csumCompare" $ nf (map testCompare) test
                ]
    print $ map testCompare test
