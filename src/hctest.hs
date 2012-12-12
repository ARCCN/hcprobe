{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Openflow.Types
import Network.Openflow.Messages
import HCProbe.FakeSwitch

import Data.Binary.Put ( runPut )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import Data.Word
import Text.Printf
import Data.Maybe
import Data.List (intersperse, concat, unfoldr)

import System.Random
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


helloBs xid = bsStrict $ runPut (ofpHelloRequest openflow_1_0 xid)

encodeMsg = bsStrict . runPut . putMessage

--featuresReplBs sw xid = bsStrict $ runPut $ do
--  let hdr = header openflow_1_0 xid OFPT_FEATURES_REPLY
--  let feature_repl = OfpMessage hdr (OfpFeatureReply sw)
--  putMessage feature_repl

ofpClient sw host port = runTCPClient (clientSettings port host) (client sw)

client :: OfpSwitchFeatures -> Application IO
client sw ad = appSource ad $$ conduit
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
        OFPT_FEATURES_REQUEST -> do let repl = featuresReply openflow_1_0 sw (ofp_hdr_xid hdr)
                                    let resp = encodeMsg repl
                                    liftIO $ dump "OUT:" (ofp_header repl) resp
                                    lift $ yield resp $$ (appSink ad)
        _          -> return () 

--    dump :: String -> BS.ByteString -> OfpHeader -> Application a
dump :: String -> OfpHeader -> BS.ByteString -> IO ()
dump s hdr bs = do
  let tp = show (ofp_hdr_type hdr)
  putStr $ printf "%-4s %-24s %s\n" s tp (hexdumpBs 32 " " "" (BS.take 32 bs))


main :: IO ()
main = do
  (host:port:_) <- getArgs
  rnd <- newStdGen
--  let (p,g) = makePort (defaultGen rnd) [] [] [OFPPF_1GB_HD,OFPPF_COPPER]
  let (sw,g') = makeSwitch (defaultSwGen rnd) 48 [] [] [] [OFPPF_1GB_HD,OFPPF_COPPER]
  let hdr = header openflow_1_0 1 OFPT_FEATURES_REPLY
  let feature_repl = OfpMessage hdr (OfpFeatureReply sw)
  let bs = bsStrict $ runPut (putMessage feature_repl)
  putStrLn (fmtSwitch sw)
--  putStr $ hexdumpBs 16 " " "\n" bs
--  dump "TEST" hdr bs
--  print p
--  let sw = defaultSwitchFeatures 1 48
--  putStr (hexdumpBs 16 " " "\n" helloBs)
  ofpClient sw (BS8.pack host) (read port)
  putStrLn "done"


