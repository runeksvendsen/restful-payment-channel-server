{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, LambdaCase, TypeOperators #-}

module PayChanServer where

import           AppPrelude.Util (runLocalhost, envRead)
import qualified PayChanServer.API as API
import qualified PayChanServer.App as App
import           PayChanServer.Init (appConfInit, installHandlerKillThreadOnSig)
import           PayChanServer.Settlement (settlementThread)
import qualified PayChanServer.Config.Types as Conf

import           PayChanServer.Types
import           PayChanServer.Config.Util
import           Servant

import           System.Environment (lookupEnv, getArgs, getProgName)
import           Text.Read (readMaybe)

import qualified System.Posix.Signals as Sig
import           Control.Concurrent (forkIO, myThreadId)
import           Data.Maybe (fromMaybe)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Monad.Reader as Reader

-- Profile
import           Test.Profile (profile_selfDestruct)

readerToEither :: DB.DBConf db => Conf.App db -> AppPC :~> Handler
readerToEither cfg = Nat $ \x -> Reader.runReaderT x cfg


apiRBPCP :: Proxy API.RBPCP
apiRBPCP = Proxy

apiMan :: Proxy API.Man
apiMan = Proxy

payChanApp :: DB.DBConf db => Conf.App db -> Wai.Application
payChanApp cfg = serve apiRBPCP $ serverEmbedConf App.payChanServer cfg
    where serverEmbedConf server cfg = enter (readerToEither cfg) server

managementApp :: DB.DBConf db => Conf.App db -> Wai.Application
managementApp cfg = serve apiMan $ serverEmbedConf App.managementServer cfg
    where serverEmbedConf server cfg = enter (readerToEither cfg) server

main :: IO ()
main = wrapArg $ \cfg _ -> do
    --  Start thread that settles channels before expiration date
    _ <- forkIO $ startSettlementThread cfg (60 * 5)  -- run every 5 minutes
    -- Shut down on TERM signal
    myThreadId >>= installHandlerKillThreadOnSig Sig.sigTERM
    -- Start server
    runApp cfg

runApp :: Conf.Config -> IO ()
runApp cfg = do
    --  Get port from PORT environment variable, if it contains a valid port number
    maybePort <- envRead "PORT"
    appConf <- appConfInit cfg
    --  Start management interface
    port <- getManagementIfacePort cfg
    forkIO $ runManIface port appConf
    --  Start paychan app
    Warp.run (fromIntegral . fromMaybe 8080 $ maybePort) (payChanApp appConf)

runManIface :: DB.DBConf db => Word -> Conf.App db -> IO ()
runManIface port appConf = do
    putStrLn $ "Starting management interface on port " ++ show port
    runLocalhost port (managementApp appConf)

-- |Close payment channels before we reach the expiration date,
--  because if we don't, the client can reclaim all the funds sent to us,
--  leaving us with nothing.
startSettlementThread cfg i = do
    dbIface <- getChanStoreIface =<< getDBConf cfg
    settleConf <- getServerSettleConfig cfg
    signConn <- getSigningServiceConn cfg
    btcIface <- getBlockchainIface cfg
    putStrLn "Started settlement thread." >>
        settlementThread dbIface signConn settleConf btcIface i

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