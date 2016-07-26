{-# LANGUAGE OverloadedStrings #-}

module Server.Init where


import           Server.App  (mainRoutes)

import           Server.Config
import           Server.Config.Types
import           Server.Types (OpenConfig(..), ServerSettleConfig(..))

import           Common.Common (pathParamEncode)
import           Data.String.Conversions (cs)

import           ChanStoreServer.ChanStore.Types (ConnManager)
import           Server.Settlement (settleChannel)
import           SigningService.Interface (getPubKey)

import           Snap (SnapletInit, makeSnaplet, addRoutes)

import           Control.Monad.IO.Class (liftIO)
import qualified System.Posix.Signals as Sig
import           Control.Concurrent (ThreadId)
import           Control.Concurrent (throwTo)
import qualified Control.Exception as E

appInit :: Config -> ConnManager -> SnapletInit App App
appInit cfg databaseConn = makeSnaplet "PayChanServer" "RESTful Bitcoin payment channel server" Nothing $ do

    bitcoinNetwork <- liftIO (configLookupOrFail cfg "bitcoin.network")
    liftIO $ setBitcoinNetwork bitcoinNetwork

    (ServerSettleConfig settleFee settlePeriod) <- liftIO $ getServerSettleConfig cfg

    (OpenConfig minConfOpen basePrice addSettleFee) <- OpenConfig <$>
            liftIO (configLookupOrFail cfg "open.fundingTxMinConf") <*>
            liftIO (configLookupOrFail cfg "open.basePrice") <*>
            liftIO (configLookupOrFail cfg "open.priceAddSettlementFee")
    let confOpenPrice = if addSettleFee then basePrice + settleFee else basePrice

    signingServiceConn <- liftIO $ getSigningServiceConn cfg
    bitcoindRPCConf <- liftIO $ getBitcoindConf cfg
    let settleChanFunc = settleChannel databaseConn signingServiceConn bitcoindRPCConf settleFee

    liftIO $ putStr $ "Contacting SigningService to get public key..."
    pubKey <- liftIO $ getPubKey signingServiceConn
    liftIO . putStrLn $ "PubKey: " ++ cs (pathParamEncode pubKey)

    let basePathVersion = "/v1"
    addRoutes $ mainRoutes basePathVersion

    return $ App databaseConn pubKey
                 confOpenPrice settlePeriod minConfOpen
                 basePathVersion settleChanFunc


installHandlerKillThreadOnSig :: Sig.Signal -> ThreadId -> IO Sig.Handler
installHandlerKillThreadOnSig sig tid =
    Sig.installHandler
          sig
          (Sig.CatchInfo $ \ci -> do
              putStrLn ("Caught signal: " ++
                  show (Sig.siginfoSignal ci) ++
                  ". Killing main thread...")
              throwTo tid E.UserInterrupt)
          Nothing --(Just Sig.fullSignalSet)
