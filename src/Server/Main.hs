{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Server.Init (appInit)
import           Snap (serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)
import           System.Environment (lookupEnv, getArgs, getProgName)
import           Text.Read (readMaybe)
import           Snap.Http.Server.Config (setPort)

import           Control.Monad (unless)
import           Control.Monad.Catch (bracket, finally)
import           Control.Concurrent (forkIO, throwTo, myThreadId)
import qualified Control.Exception as E
import qualified System.Posix.Signals as Sig -- (Handler(Catch), installHandler, sigTERM)
import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)

import           Server.ChanStore (ChannelMap, newChanMap, mapLen, diskSyncThread, diskSyncNow,
                                    sync_chanMap, init_chanMap)
import           DiskStore (syncMapToDisk)
import qualified Server.ChanStore.Connection as DB
import           Server.Config (loadConfig, configLookupOrFail)
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
    chanStoreDir <- loadConfig cfgFilePath >>= flip configLookupOrFail "storage.stateDir"

    map <- init_chanMap chanStoreDir -- "/opt/paychan/state/test/"
    syncThread <- forkIO (diskSyncThread map 5)
    conn <- DB.newChanMapConnection map
    -- we need to kill the main thread manually since we capture kill signals
    mainThread <- myThreadId

    prevHandler <- Sig.installHandler
            Sig.sigTERM
            (Sig.CatchInfo $ \ci -> do
                putStrLn ("Preparing shutdown. Caught signal: " ++
                    show (Sig.siginfoSignal ci))
                throwTo mainThread E.UserInterrupt)
            (Just Sig.fullSignalSet)
    --      first do this    at the end do this always             after doing this
    bracket (return conn) (handleShutdown syncThread map) (runApp $ F.dropExtension cfgFilePath)


handleShutdown :: ThreadId -> ChannelMap -> DB.ChanMapConn -> IO ()
handleShutdown syncThread map conn = do
    DB.closeConnection conn
    killThread syncThread
    sync_chanMap map

runApp :: String -> DB.ChanMapConn -> IO ()
runApp env chanMapConn = do
    (_, app, _) <- runSnaplet (Just env) (appInit chanMapConn)

    maybePort <- return . maybe Nothing readMaybe =<< lookupEnv "PORT"

    let conf = case maybePort :: Maybe Word of
            Nothing     -> defaultConfig
            Just port   -> setPort (fromIntegral port) defaultConfig

    httpServe conf app

