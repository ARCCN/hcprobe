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

bsFromW16 w16 = BP.runPut $ BP.putBuilder $ foldl accPutWord16be BB.empty w16
    where accPutWord16be acc w = acc `BB.append` (BB.putWord16be w)

test :: [Word16] -> Bool
test a = (testNew a) == (testOld a)
{-
testNew lst = 
    (icsum16 0 . V.fromList) <$> lst

testOld lst = 
    (icsum16' 0 . BS.pack) <$> lst -}

genLengthDEF = 10000 :: Int

main = do
    test <- arbitary

    defaultMain [ bgrup "csumNew" $ nf testNew (V.fromList arbitary)
                , bgrup "csumOld" $ nf testOld (bsfromW16 arbitary)
                ]
