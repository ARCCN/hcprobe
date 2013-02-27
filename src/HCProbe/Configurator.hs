{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module HCProbe.Configurator 
( Parameters (..)
, logFileName
, getParameters
) where

-- TODO see line 38

import System.Console.CmdArgs
import System.IO

import qualified Data.ConfigFile as CF
import Data.Either.Utils

import Control.Monad
import Control.Monad.State
import Control.Monad.Maybe
import Control.Monad.Error

--Default falues
macSpaceDim_DEF    = 100
switchNum_DEF      = 64
maxTimeout_DEF     = 10
payloadLen_DEF     = 32
samplingPeriod_DEF = 300000
statsDelay_DEF     = 300000
testDuration_DEF   = 300*1000000
logFileName_DEF    = ""

pktInQLen_DEF     = 10000
pktInQTimeout_DEF = 0.5

host_DEF	= "127.0.0.1"
port_DEF	= "6633"

--Default config filepath
-- TODO try to make program to read it from cmd args.
confFilePath	= "hcprobe.conf"

--Structure with parameters
data Parameters = Parameters
	{ macSpaceDim		:: Int
	, switchNum		:: Int
	, maxTimeout		:: Int
	, payloadLen		:: Int
	, samplingPeriod	:: Int
	, statsDelay		:: Int
	, testDuration		:: Int
	, logFileName'		:: String
	
	, pktInQLen	:: Int
	, pktInQTimeout	:: Float
	
	, host	:: String
	, port	:: String
	} deriving (Show, Data, Typeable)
	
-- Program use Maybe String to get logFileName value
-- We can't use Maybe type in cmdArgs lib, so we say that
-- empty string mean NO logFile 
-- TODO logFileName' shouldn't be accesible from other program
logFileName :: Parameters => Maybe String
logFileName param  = if logFileName' param == ""
			then Nothing
			else Just $ logFileName' param 

-- Setting Parameters struture to use with cmdArgs
-- TODO make fine help
parametersDef = Parameters { macSpaceDim = macSpaceDim_DEF 		&= help "MAC Space Dim" 	&= typ "NUM"
			   , switchNum = switchNum_DEF 			&= help "Number of swithes" 	&= typ "NUM"
			   , maxTimeout = maxTimeout_DEF 		&= help "Maximum timeout" 	&= typ "NUM"
			   , payloadLen = payloadLen_DEF 		&= help "Payload length" 	&= typ "NUM"
			   , samplingPeriod = samplingPeriod_DEF 	&= help "Sampling period" 	&= typ "NUM"
			   , statsDelay = statsDelay_DEF 		&= help "Stats delay"	 	&= typ "NUM"
			   , testDuration = testDuration_DEF 		&= help "Test duration" 	&= typ "NUM"
			   , logFileName' = logFileName_DEF 		&= help "Log path"	 	&= typ "PATH"
			   
			   , pktInQLen = pktInQLen_DEF		&= help "In packet Q length" 	&= typ "NUM"
			   , pktInQTimeout = pktInQTimeout_DEF 	&= help "In packet Q timeout" 	&= typ "RATIONAL"
			   
			   , host = host_DEF 	&= help "Controller's host" 	&= typ "ADDRES"
			   , port = port_DEF 	&= help "Controller's port" 	&= typ "NUM"
			   } &=
			   help "HCProbe"

--Read parameters from config file
parametersConf :: Parameters -> IO Parameters
parametersConf params = do
	val <- CF.readfile CF.emptyCP confFilePath
        let cp = forceEither val 
        foldM  applyConf params (forceEither $ CF.items cp "DEFAULT")
	where applyConf :: Parameters -> (CF.OptionSpec, String) -> IO Parameters
	      applyConf params ("macspacedim",val) = return $ params { macSpaceDim = read val}
	      applyConf params ("switchnum",val) = return $ params { switchNum = read val}
	      applyConf params ("maxtimeout",val) = return $ params { maxTimeout = read val}
	      applyConf params ("payloadlen",val) = return $ params { payloadLen = read val}
	      applyConf params ("samplingperiod",val) = return $ params { samplingPeriod = read val}
	      applyConf params ("statsdelay",val) = return $ params { statsDelay = read val}
	      applyConf params ("testduration",val) = return $ params { testDuration = read val}
	      applyConf params ("logfilename",val) = return $ params { logFileName' = val}
	      
	      applyConf params ("pktinqlen",val) = return $ params { pktInQLen = read val}
	      applyConf params ("pktinqtimeout",val) = return $ params { pktInQTimeout = read val}
	      
	      applyConf params ("host",val) = return $ params { host = val}
	      applyConf params ("port",val) = return $ params { port = val}
	      applyConf params (_,_) = return params

	
			   
getParameters :: IO Parameters
getParameters = return parametersDef >>= parametersConf >>= cmdArgs