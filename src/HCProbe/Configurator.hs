{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module HCProbe.Configurator 
( Parameters (..)
, logFileName
, getParameters
) where
	
import System.Console.CmdArgs
import System.IO

import qualified Data.ConfigFile as CF
import Data.Either.Utils

import Control.Monad
import Control.Monad.State
import Control.Monad.Maybe

macSpaceDim_DEF    = 100
switchNum_DEF      = 64
maxTimeout_DEF     = 10
payloadLen_DEF     = 32
samplingPeriod_DEF = 300000
statsDelay_DEF     = 300000
testDuration_DEF   = 300*1000000
logFileName_DEF    = "report.log"

pktInQLen_DEF     = 10000
pktInQTimeout_DEF = 0.5

host_DEF	= "127.0.0.1"
port_DEF	= "6633"

confFilePath	= "hcprobe.conf"

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
	
logFileName :: Parameters => Maybe String
logFileName param  = if logFileName' param == ""
			then Nothing
			else Just $ logFileName' param 

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

parametersConf :: Parameters -> IO Parameters
parametersConf params = do
	val <- CF.readfile CF.emptyCP confFilePath
        let cp = forceEither val 
        res <- if (CF.has_option cp "DEFAULT" "macSpaceDim")
		then do putStrLn "YES"
			return params { macSpaceDim = forceEither $ CF.get cp "DEFAULT" "macSpaceDim" } 
		else do putStrLn "NO"
			return params
	if (CF.has_option cp "DEFAULT" "switchNum")
		then return res { switchNum = forceEither $ CF.get cp "DEFAULT" "switchNum" } 
		else return res
-- 	res <- when (has_option cp "DEFAULT" "macSpaceDim")
-- 		return res { macSpaceDim = forceEither $ get cp "DEFAULT" "macSpaceDim" } 
-- 	res <- when (has_option cp "DEFAULT" "macSpaceDim")
-- 		return res { macSpaceDim = forceEither $ get cp "DEFAULT" "macSpaceDim" } 

	
			   
getParameters :: IO Parameters
getParameters = do 
	param <- parametersConf parametersDef
	cmdArgs param