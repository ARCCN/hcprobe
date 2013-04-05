{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit.BinaryParse
  ( conduitBinary
  )
  where

import           Control.Exception
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString (ByteString)

import           Data.Conduit
import qualified Data.Conduit.List  as CL
import           Data.Typeable


data ParseError = ParseError
      { unconsumed :: ByteString
      , offset     :: ByteOffset
      , content    :: String
      } deriving (Show, Typeable)

instance Exception ParseError

conduitBinary :: (Binary b, MonadThrow m) => Conduit ByteString m b
conduitBinary = 
    conduit (runGetIncremental get)
  where 
    conduit p = await >>= go . flip (maybe pushEndOfInput (flip pushChunk)) p 
                          -- \x -> go ((maybe pushEndOfInput (flip pushChunk) x) p)
        where 
          go (Done bs ofs v)       = do yield v
                                        go (runGetIncremental get `pushChunk` bs)
          go (Fail u o e)          = monadThrow (ParseError u o e)
          go n@(Partial next)      = conduit n 

