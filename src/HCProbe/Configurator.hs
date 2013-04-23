{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module HCProbe.Configurator ( Parameters (..)
                            , getParameters
                            ) where

-- TODO see line 60

import System.Console.CmdArgs hiding (Default, def)

import qualified Data.ConfigFile as CF
import Data.Either.Utils
import Data.Default
import Data.Maybe

import System.Directory

import Data.Word


--Structure with parameters
data Parameters = Parameters { config           :: Maybe String -- If user don't cpecify the config file, use default
                             , macSpaceDim      :: Word64
                             , switchNum        :: Int
                             , portNum          :: Int
                             , maxTimeout       :: Int
                             , payloadLen       :: Int
                             , samplingPeriod   :: Int
                             , statsDelay       :: Int
                             , testDuration     :: Int
                             , logFileName      :: Maybe String
                             
                             , pktInQLen        :: Int
                             , pktInQTimeout    :: Float
                             
                             , host     :: String
                             , port     :: String
                             } deriving (Show, Data, Typeable)
  
configDEF :: String
configDEF = "hcprobe.conf"

instance Default Parameters where
  def = Parameters Nothing               -- config
                   100                   -- macSpaceDim
                   16                    -- switchNum
                   48                    -- port num
                   10                    -- maxtimeout
                   128                   -- payload len
                   300000                -- sampling period
                   300000                -- stats delay
                   (300*1000000)         -- test duration
                   Nothing               -- log filename
                   10000                 -- pktInQLen
                   0.5                   -- pktInQTimeout
                   "127.0.0.1"           -- host
                   "6633"                -- port

-- Setting Parameters struture to use with cmdArgs
-- TODO make fine help
parametersDef :: Parameters
parametersDef = Parameters
    { config         = config def          &= help "Config file path"                 &= typ "PATH"
    , macSpaceDim    = macSpaceDim def     &= help "MAC Space Dim (100)"              &= typ "NUM"
    , switchNum      = switchNum def       &= help "Number of swithes (64)"           &= typ "NUM"
    , portNum        = portNum def         &= help "Number of ports per Switch (48)"  &= typ "NUM"
    , maxTimeout     = maxTimeout def      &= help "Maximum timeout (10)"             &= typ "NUM"
    , payloadLen     = payloadLen def      &= help "Payload length (32)"              &= typ "NUM"
    , samplingPeriod = samplingPeriod def  &= help "Sampling period (300 000)"        &= typ "NUM"
    , statsDelay     = statsDelay def      &= help "Stats delay (300 000)"            &= typ "NUM"
    , testDuration   = testDuration def    &= help "Test duration (300 000 000)"      &= typ "NUM"
    , logFileName    = logFileName def     &= help "Log path"                         &= typ "PATH"

    , pktInQLen      = pktInQLen def       &= help "In packet Q length (1000)"        &= typ "NUM"
    , pktInQTimeout  = pktInQTimeout def   &= help "In packet Q timeout (0.5)"        &= typ "RATIONAL"
                        
    , host = host def         &= help "Controller's host (127.0.0.1)"    &= typ "ADDRES"
    , port = port def         &= help "Controller's port (6633)"         &= typ "NUM"
    } &=
      help "HCProbe"

--Read parameters from config file
parametersConf :: Parameters -> IO Parameters
parametersConf params = do
    let noExc = isNothing (config params)                --Throw exception only if no file, specified by user
    fileExists <- doesFileExist configfile
    if fileExists 
        then do
            val <- CF.readfile CF.emptyCP configfile     -- I was trying to cath the exception. No result((
            readConf val params
        else 
           if noExc 
                then return params
                else ioError $ userError ("Can not open file: " ++ configfile)
    where configfile | isNothing (config params) = configDEF
                     | otherwise = fromJust $ config params -- The better way?
            
          readConf (Right cp) ps = 
            --For every option in [(OptionSpec,String)] we set fild of our struct
            return $ foldl applyConf ps (forceEither $ CF.items cp "DEFAULT")

          readConf (Left _) _ =
                ioError $ userError "Can not read config file."

          applyConf :: Parameters -> (CF.OptionSpec, String) -> Parameters
          applyConf p ("macspacedim",val)      = p { macSpaceDim = read val}
          applyConf p ("switchnum",val)        = p { switchNum = read val}
          applyConf p ("portnum",val)          = p { portNum = read val}
          applyConf p ("maxtimeout",val)       = p { maxTimeout = read val}
          applyConf p ("payloadlen",val)       = p { payloadLen = read val}
          applyConf p ("samplingperiod",val)   = p { samplingPeriod = read val}
          applyConf p ("statsdelay",val)       = p { statsDelay = read val}
          applyConf p ("testduration",val)     = p { testDuration = read val}
          applyConf p ("logfilename",val)      = p { logFileName = Just val}
        
          applyConf p ("pktinqlen",val)        = p { pktInQLen = read val}
          applyConf p ("pktinqtimeout",val)    = p { pktInQTimeout = read val}
        
          applyConf p ("host",val)     = p { host = val}
          applyConf p ("port",val)     = p { port = val}
          applyConf p (_,_)    = p
                           
getParameters :: IO Parameters
getParameters = cmdArgs parametersDef >>= parametersConf >>= cmdArgs
