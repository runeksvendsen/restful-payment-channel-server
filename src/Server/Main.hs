{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import qualified ConnManager.Connection as DB
import           Server.Config -- (Config, loadConfig, configLookupOrFail, getSettleConfig, getBitcoindConf, getDBConf)
import           Server.Init (appInit, installHandlerKillThreadOnSig)
import           ChanStoreServer.ChanStore.Types (ConnManager)
import           Server.Settlement (startSettlementThread)

import           Snap (serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)
import           Snap.Http.Server.Config (setPort)

import           System.Environment (lookupEnv, getArgs, getProgName)
import           Text.Read (readMaybe)

import           Control.Monad (unless)
import           Control.Monad.Catch (bracket, finally)
import           Control.Concurrent (forkIO, throwTo, myThreadId)
import qualified Control.Exception as E
import qualified System.Posix.Signals as Sig
import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
-- import           DiskStore (syncMapToDisk)
import qualified System.FilePath as F


wrapArg :: (Config -> String -> IO ()) -> IO ()
wrapArg main' = do
    args <- getArgs
    prog <- getProgName
    if  length args < 1 then
            putStrLn $ "Usage: " ++ prog ++ " /path/to/config.cfg"
        else do
            let cfgFile = head args
            putStrLn $ "Using config file " ++ show cfgFile ++ ". Reading..."
            cfg <- loadConfig cfgFile
            main' cfg cfgFile

main :: IO ()
main = wrapArg $ \cfg cfgFilePath -> do
    dbConf <- getDBConf cfg
    dbConn <- connFromDBConf dbConf
    settleConfig <- getServerSettleConfig cfg
    settleConn <- getSigningServiceConn cfg
    bitcoindConf <- getBitcoindConf cfg
    --  Start thread which settles channels before expiration (config: settlement.settlementPeriodHours)
    _ <- forkIO $ startSettlementThread
            dbConn settleConn settleConfig bitcoindConf (60 * 5)  -- run every 5 minutes
    -- Shut down on TERM signal
    mainThread <- myThreadId
    _ <- installHandlerKillThreadOnSig Sig.sigTERM mainThread
    runApp (F.dropExtension cfgFilePath) cfg dbConn

runApp :: String -> Config -> ConnManager -> IO ()
runApp env cfg chanMapConn = do
    (_, app, _) <- runSnaplet (Just env) (appInit cfg chanMapConn)
    --  Get port from PORT environment variable, if it contains a valid port number
    maybePort <- return . maybe Nothing readMaybe =<< lookupEnv "PORT"
    let conf = case maybePort :: Maybe Word of
            Nothing     -> defaultConfig
            Just port   -> setPort (fromIntegral port) defaultConfig
    --  Start app
    httpServe conf app


