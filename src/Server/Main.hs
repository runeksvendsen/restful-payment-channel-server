{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import qualified Server.ChanStore.Connection as DB
import           Server.Config (Config, loadConfig, configLookupOrFail, getSettleConfig, getBitcoindConf)
import           Server.Init (appInit)
import           Server.ChanStore (ChannelMap, newChanMap, mapLen, diskSyncThread, diskSyncNow,
                                    sync_chanMap, init_chanMap)
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
import           DiskStore (syncMapToDisk)
import qualified System.FilePath as F

-- import qualified Control.Concurrent.MVar  as MVar

wrapArg :: (String -> IO ()) -> IO ()
wrapArg main' = do
    args <- getArgs
    prog <- getProgName
    if  length args < 1 then
            putStrLn $ "Usage: " ++ prog ++ " /path/to/config.cfg"
        else
            main' $ head args

main :: IO ()
main = wrapArg $ \cfgFilePath -> do
    cfg <- loadConfig cfgFilePath

    chanStoreDir <- configLookupOrFail cfg "storage.stateDir"
    map <- init_chanMap chanStoreDir
    syncThread <- forkIO (diskSyncThread map 5)

    settleConf <- getSettleConfig cfg
    bitcoindConf <- getBitcoindConf cfg
    settleThread <- forkIO $ settlementThread

    conn <- DB.newChanMapConnection map
    -- we need to kill the main thread manually since we capture kill signals
    mainThread <- myThreadId

    prevHandler <- Sig.installHandler
            Sig.sigTERM
            (Sig.CatchInfo $ \ci -> do
                putStrLn ("Caught signal: " ++
                    show (Sig.siginfoSignal ci) ++
                    ". Killing main thread...")
                throwTo mainThread E.UserInterrupt)
            (Just Sig.fullSignalSet)
    --      1. first do this  3. at the end always do this
    bracket (return conn)     (handleShutdown syncThread map) $
            runApp (F.dropExtension cfgFilePath) cfg --- <--- 2. do this in-between


handleShutdown :: ThreadId -> ChannelMap -> DB.ChanMapConn -> IO ()
handleShutdown syncThread map conn = do
    DB.closeConnection conn
    killThread syncThread
    sync_chanMap map

runApp :: String -> Config -> DB.ChanMapConn -> IO ()
runApp env cfg chanMapConn = do
    (_, app, _) <- runSnaplet (Just env) (appInit cfg chanMapConn)

    maybePort <- return . maybe Nothing readMaybe =<< lookupEnv "PORT"

    let conf = case maybePort :: Maybe Word of
            Nothing     -> defaultConfig
            Just port   -> setPort (fromIntegral port) defaultConfig

    httpServe conf app

