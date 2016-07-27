{-# LANGUAGE OverloadedStrings #-}

module Server.Init where


import           Server.App  (mainRoutes)

import           Server.Config
import           Server.Config.Types
import           Server.Types (OpenConfig(..), ServerSettleConfig(..))
import           Server.Util (dummyKey)
import           Server.Settlement (settleChannel)

import           Server.DB (tryDBRequest)
import           ChanStoreServer.Interface  as DBConn
import           SigningService.Interface (getPubKey)

import           Common.Common (pathParamEncode)
import           ChanStoreServer.ChanStore.Types (ConnManager)

import           Snap (SnapletInit, makeSnaplet, addRoutes)
import           Data.String.Conversions (cs)
import           Control.Monad.IO.Class (liftIO)
import qualified System.Posix.Signals as Sig
import           Control.Concurrent (ThreadId)
import           Control.Concurrent (throwTo)
import qualified Control.Exception as E
import           Data.Maybe (isNothing)


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

    maybeRes <- liftIO $ DBConn.chanGet databaseConn dummyKey
    liftIO $ putStr $ "Testing database connection... "
    liftIO $ putStrLn $ if isNothing maybeRes then "success." else "something is horribly broken"

    pubKey <- liftIO $ getPubKey signingServiceConn
    liftIO $ putStr $ "Contacting SigningService for public key... "
    liftIO $ putStrLn $ "success: " ++ cs (pathParamEncode pubKey)

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
              putStrLn ("Received signal: " ++
                  show (Sig.siginfoSignal ci) ++
                  ". Killing main thread...")
              throwTo tid E.UserInterrupt)
          Nothing --(Just Sig.fullSignalSet)
