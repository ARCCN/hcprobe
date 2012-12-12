{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types ( OfpHeader(..), OfpType(..), OfpMessage(..) )
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
      when (isJust bs') $ do
        let bs = fromJust bs'
        case (ofpParsePacket bs) of
          Just (msg, rest) -> (liftIO $ dump "IN:" (ofp_header msg) bs) >> processMessage msg >> leftover rest
          Nothing          -> return ()
      conduit

    processMessage (OfpMessage hdr msg) = do
      case (ofp_hdr_type hdr) of
        OFPT_HELLO -> do let resp = helloBs (ofp_hdr_xid hdr)
                         liftIO $ dump "OUT:" hdr resp
                         lift $ yield resp $$ (appSink ad)
        _          -> return () 

--    dump :: String -> BS.ByteString -> OfpHeader -> Application a
dump :: String -> OfpHeader -> BS.ByteString -> IO ()
dump s hdr bs = do
  let tp = show (ofp_hdr_type hdr)
  putStr $ printf "%-4s %-24s %s\n" s tp (take 64 (hexdumpBs 64 " " "" bs))


main :: IO ()
main = do
  (host:port:_) <- getArgs
--  putStr (hexdumpBs 16 " " "\n" helloBs)
  ofpClient (BS8.pack host) (read port)



