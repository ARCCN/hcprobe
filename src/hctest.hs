{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types ( OfpHeader(..), OfpType(..) )
import Network.Openflow.Messages
import Data.Binary.Put ( runPut )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Text.Printf
import Data.Maybe
import Data.List (intersperse, concat, unfoldr)

import System.Environment (getArgs)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Conduit.Network

hexdumpBs :: Int -> String -> String -> BS.ByteString -> String
hexdumpBs n ds ts bs = concat $ concat rows
  where hexes :: [String]
        hexes = reverse $ BS.foldl (\acc w -> (printf "%02X" w :: String) : acc) [] bs
        rows  = unfoldr chunk hexes
        chunk [] = Nothing
        chunk xs = Just (intersperse ds (take n xs) ++ [ts], drop n xs)


helloBs xid = bsStrict $ runPut (ofpHelloRequest 0x01 xid)

ofpClient host port = runTCPClient (clientSettings port host) client

client :: Application IO
client ad = appSource ad $$ conduit
  where
    conduit = do
      bs' <- await
      let hdr' = bs' >>= ofpParseHeader
      when (isJust hdr') $ do
        let hdr = fromJust hdr'
        let bs  = fromJust bs'
        let msg = show (ofp_hdr_type hdr)
        liftIO $ printf "IN MSG: %-16s %s\n" msg (take 32 (hexdumpBs 32 " " "" bs))
        case (ofp_hdr_type hdr) of
          OFPT_HELLO -> do lift $ yield (helloBs (ofp_hdr_xid hdr)) $$ (appSink ad)
          _          -> return ()
      conduit


main :: IO ()
main = do
  (host:port:_) <- getArgs
--  putStr (hexdumpBs 16 " " "\n" helloBs)
  ofpClient (BS8.pack host) (read port)



