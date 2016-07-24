{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import qualified Server.ChanStore.Connection as DB
import           Server.Config -- (Config, loadConfig, configLookupOrFail, getSettleConfig, getBitcoindConf, getDBConf)
import           Server.Init (appInit, installHandlerKillThreadOnSig)
import           Server.ChanStore.Types (ConnManager)
import           Server.ChanStore (newChanMap, mapLen, -- diskSyncThread, diskSyncNow, sync_chanMap,
                                    init_chanMap)
import           Server.TimeSettlement (settlementThread)

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

    settleConfig <- getServerSettleConfig cfg
    settleConn <- getSigningServiceConn cfg

    mainThread <- myThreadId
    _ <- installHandlerKillThreadOnSig Sig.sigTERM mainThread
    --       1. first do this            3. at the end always do this
    bracket  (connFromDBConf dbConf)     handleShutdown $
             runApp (F.dropExtension cfgFilePath) cfg --- <--- 2. do this in-between

handleShutdown :: ConnManager -> IO ()
handleShutdown conn = return ()
--     DB.closeConnection conn
--     killThread syncThread
--     sync_chanMap map

runApp :: String -> Config -> ConnManager -> IO ()
runApp env cfg chanMapConn = do
    (_, app, _) <- runSnaplet (Just env) (appInit cfg chanMapConn)
    maybePort <- return . maybe Nothing readMaybe =<< lookupEnv "PORT"

    let conf = case maybePort :: Maybe Word of
            Nothing     -> defaultConfig
            Just port   -> setPort (fromIntegral port) defaultConfig

    httpServe conf app


