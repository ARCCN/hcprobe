{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module HCProbe.Configurator ( Parameters (..)
                            , getParameters
                            ) where

-- TODO see line 60

import System.Console.CmdArgs
import System.IO

import qualified Data.ConfigFile as CF
import Data.Either.Utils
import Data.Maybe

import Control.Monad
import Control.Monad.State
import Control.Monad.Maybe
import Control.Monad.Error
import Control.Exception
import System.Directory

import Data.Word

--Default values
macSpaceDimDEF    = 100
switchNumDEF      = 64
portNumDEF        = 48
maxTimeoutDEF     = 10
payloadLenDEF     = 32
samplingPeriodDEF = 300000
statsDelayDEF     = 300000
testDurationDEF   = 300*1000000
logFileNameDEF    = Nothing

pktInQLenDEF     = 10000
pktInQTimeoutDEF = 0.5

hostDEF        = "127.0.0.1"
portDEF        = "6633"

--Default config filepath
configDEF        = "hcprobe.conf"

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

-- Setting Parameters struture to use with cmdArgs
-- TODO make fine help
parametersDef = Parameters { config = Nothing                          &= help "Config file path"           &= typ "PATH"
                           , macSpaceDim = macSpaceDimDEF              &= help "MAC Space Dim"              &= typ "NUM"
                           , switchNum = switchNumDEF                  &= help "Number of swithes"          &= typ "NUM"
                           , portNum = portNumDEF                      &= help "Number of ports per Switch" &= typ "NUM"
                           , maxTimeout = maxTimeoutDEF                &= help "Maximum timeout"            &= typ "NUM"
                           , payloadLen = payloadLenDEF                &= help "Payload length"             &= typ "NUM"
                           , samplingPeriod = samplingPeriodDEF        &= help "Sampling period"            &= typ "NUM"
                           , statsDelay = statsDelayDEF                &= help "Stats delay"                &= typ "NUM"
                           , testDuration = testDurationDEF            &= help "Test duration"              &= typ "NUM"
                           , logFileName = logFileNameDEF              &= help "Log path"                   &= typ "PATH"
                           
                           , pktInQLen = pktInQLenDEF             &= help "In packet Q length"         &= typ "NUM"
                           , pktInQTimeout = pktInQTimeoutDEF     &= help "In packet Q timeout"        &= typ "RATIONAL"
                           
                           , host = hostDEF         &= help "Controller's host"         &= typ "ADDRES"
                           , port = portDEF         &= help "Controller's port"         &= typ "NUM"
                           } &=
                           help "HCProbe"

--Read parameters from config file
parametersConf :: Parameters -> IO Parameters
parametersConf params = do
    let noExc = isNothing (config params)                --Throw exception only if no file, specified by user
    fileExists <- doesFileExist $ configfile params 
    if fileExists 
        then do
            val <- CF.readfile CF.emptyCP $ configfile params  -- I was trying to cath the exception. No result((
            readConf val params
        else 
           if noExc 
                then return params
                else ioError $ userError ("Can not open file: " ++ configfile params)
    where configfile params
                    | isNothing (config params) = configDEF
                    | otherwise = fromJust $ config params -- The better way?
            
          readConf (Right cp) params = 
            --For every option in [(OptionSpec,String)] we set fild of our struct
            foldM  applyConf params (forceEither $ CF.items cp "DEFAULT")

          readConf (Left _) params =
                ioError $ userError "Can not read config file."

          applyConf :: Parameters -> (CF.OptionSpec, String) -> IO Parameters
          applyConf params ("macspacedim",val)      = return $ params { macSpaceDim = read val}
          applyConf params ("switchnum",val)        = return $ params { switchNum = read val}
          applyConf params ("portnum",val)          = return $ params { portNum = read val}
          applyConf params ("maxtimeout",val)       = return $ params { maxTimeout = read val}
          applyConf params ("payloadlen",val)       = return $ params { payloadLen = read val}
          applyConf params ("samplingperiod",val)   = return $ params { samplingPeriod = read val}
          applyConf params ("statsdelay",val)       = return $ params { statsDelay = read val}
          applyConf params ("testduration",val)     = return $ params { testDuration = read val}
          applyConf params ("logfilename",val)      = return $ params { logFileName = Just val}
        
          applyConf params ("pktinqlen",val)        = return $ params { pktInQLen = read val}
          applyConf params ("pktinqtimeout",val)    = return $ params { pktInQTimeout = read val}
        
          applyConf params ("host",val)     = return $ params { host = val}
          applyConf params ("port",val)     = return $ params { port = val}
          applyConf params (_,_)    = return params

        
                           
getParameters :: IO Parameters
getParameters = cmdArgs parametersDef >>= parametersConf >>= cmdArgs
