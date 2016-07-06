{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Server.Init (appInit)
import           Snap (serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)
import           Snap.Http.Server.Config (setPort)

import           Control.Monad (unless)
import           Control.Monad.Catch (bracket, finally)
import           Control.Concurrent (forkIO, throwTo, myThreadId)
import qualified Control.Exception as E
import qualified System.Posix.Signals as Sig -- (Handler(Catch), installHandler, sigTERM)
import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)

import           Server.ChanStore (ChannelMap, newChanMap, mapLen, diskSyncThread, diskSyncNow)
import           DiskStore (syncMapToDisk)
import qualified Server.ChanStore.Connection as DB
-- import           Server.ChanStore.Client
import qualified Control.Concurrent.MVar  as MVar


main :: IO ()
main = do
    map <- init_chanMap "/opt/paychan/state/test/" -- 5 theApp
    syncThread <- forkIO (diskSyncThread map 5)
    mainThread <- myThreadId
    conn <- DB.newChanMapConnection map

    prevHandler <- Sig.installHandler
            Sig.sigTERM
            (Sig.CatchInfo $ \ci -> do
                putStrLn ("Preparing shutdown. Caught signal: " ++
                    show (Sig.siginfoSignal ci))
                throwTo mainThread E.UserInterrupt)
            (Just Sig.fullSignalSet)

    bracket (return conn) (handleShutdown syncThread map) theApp

handleShutdown :: ThreadId -> ChannelMap -> DB.ChanMapConn -> IO ()
handleShutdown syncThread map conn = do
    DB.closeConnection conn
    killThread syncThread
    sync_chanMap map

init_chanMap :: String -> IO ChannelMap
init_chanMap storageDirectory  = do
    map <- newChanMap storageDirectory
    chanMapLen <- mapLen map
    putStrLn $ "Restored " ++ show chanMapLen ++ " open channel states from " ++ show storageDirectory
    return map

sync_chanMap :: ChannelMap -> IO ()
sync_chanMap map = do
    putStrLn "Syncing channel map to disk before shutting down..."
    syncCount <- syncMapToDisk map
    putStrLn $ "Synced " ++ show syncCount ++ " channel state(s) to disk."

-- theApp :: DB.ChanMapConn -> MVar.MVar () -> IO ()
theApp :: DB.ChanMapConn -> IO ()
theApp chanMapConn = do
    let env = "/Users/rune/IdeaProjects/restful-payment-channel-server/config/test/config/server"
    (_, app, _) <- runSnaplet (Just env) (appInit chanMapConn)

    maybePort <- return . maybe Nothing readMaybe =<< lookupEnv "PORT"

    let conf = case maybePort :: Maybe Word of
            Nothing     -> defaultConfig
            Just port   -> setPort (fromIntegral port) defaultConfig

    httpServe conf app





-- runWithChanMap :: String -> Int -> (ChannelMap -> IO ()) -> IO ThreadId
-- runWithChanMap storageDirectory syncIntervalSeconds runner = do
--     bracket
--         (init_chanMap storageDirectory)
--         sync_chanMap
--         (\map -> do
--                 tid <- forkIO (diskSyncThread map syncIntervalSeconds)
--                 forkIO $ runner map
--                 return tid)
