{-# LANGUAGE OverloadedStrings #-}

module  PayChanServer.Main where

import           Common.Util -- (Config, loadConfig, configLookupOrFail, getSettleConfig, getBitcoindConf, getDBConf)
import           PayChanServer.Init (appInit, installHandlerKillThreadOnSig)
import           ConnManager.Types (ConnManager)
import           PayChanServer.Settlement (settlementThread)
import           PayChanServer.Config.Util

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
import qualified System.FilePath as F


wrapArg :: (Config -> String -> IO ()) -> IO ()
wrapArg main' = do
    args <- getArgs
    prog <- getProgName
    if  length args < 1 then
            putStrLn $ "Usage: " ++ prog ++ " /path/to/config.cfg"
        else do
            let cfgFile = head args
            putStrLn $ "Using config file " ++ show cfgFile
            cfg <- loadConfig cfgFile
            main' cfg cfgFile

main :: IO ()
main = wrapArg $ \cfg cfgFilePath -> do
    dbConn <- connFromDBConf =<< getDBConf cfg
    --  Start thread that settles channels before expiration date
    _ <- forkIO $ startSettlementThread cfg dbConn (60 * 5)  -- run every 5 minutes
    -- Shut down on TERM signal
    mainThread <- myThreadId
    _ <- installHandlerKillThreadOnSig Sig.sigTERM mainThread
    -- Start server
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

-- |Close payment channels before we reach the expiration date,
--  because if we don't, the client can reclaim all the funds sent to us,
--  leaving us with nothing.
startSettlementThread cfg dbConn i = do
    settleConf <- getServerSettleConfig cfg
    signConn <- getSigningServiceConn cfg
    bitcoindConf <- getBitcoindConf cfg
    putStrLn "Started settlement thread." >>
        settlementThread dbConn signConn settleConf bitcoindConf i