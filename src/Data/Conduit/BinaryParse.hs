{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit.BinaryParse
  ( conduitBinary
  )
  where

import           Control.Exception
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S

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
conduitBinary = start
  where 
    start = do mx <- await
               case mx of
                 Nothing -> return ()
                 Just x -> go (runGetIncremental get `pushChunk` x)
    go (Done bs _ v) = do yield v
                          if S.null bs 
                              then start
                              else go (runGetIncremental get `pushChunk` bs)
    go (Fail u o e)  = monadThrow (ParseError u o e)
    go p@(Partial _)   = conduit p
    conduit p = await >>= go . flip (maybe pushEndOfInput (flip pushChunk)) p
