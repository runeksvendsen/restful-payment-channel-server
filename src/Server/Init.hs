{-# LANGUAGE OverloadedStrings #-}

module Server.Init where



import           Server.App  (mainRoutes)

import           Server.Config
import           Server.Config.Types
import           Server.Types (OpenConfig(..), ChanSettleConfig(..))

import           Bitcoind (BTCRPCInfo(..), bitcoindNetworkSumbitTx)

import           Common.Common (pathParamEncode)
import           Data.String.Conversions (cs)

import           Server.ChanStore.Client (ChanMapConn)

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (forkIO, killThread,     threadDelay)
import qualified System.Posix.Signals as Sig (Handler(Catch), installHandler, sigTERM)

import qualified Network.Haskoin.Crypto as HC
import           Snap (SnapletInit, makeSnaplet, addRoutes)


appInit :: Config -> ChanMapConn -> SnapletInit App App
appInit cfg chanOpenMap = makeSnaplet "PayChanServer" "RESTful Bitcoin payment channel server" Nothing $ do
    --- CONFIG ---
--     cfg <- getSnapletUserConfig

    bitcoinNetwork <- liftIO (configLookupOrFail cfg "bitcoin.network")
    liftIO $ setBitcoinNetwork bitcoinNetwork

    cfgSettleConfig@(SettleConfig _ _ settleFee _) <- liftIO $ getSettleConfig cfg

    (OpenConfig minConf basePrice addSettleFee) <- OpenConfig <$>
            liftIO (configLookupOrFail cfg "open.fundingTxMinConf") <*>
            liftIO (configLookupOrFail cfg "open.basePrice") <*>
            liftIO (configLookupOrFail cfg "open.priceAddSettlementFee")

    bitcoindRPCConf <- liftIO $ getBitcoindConf cfg
    let pushTxFunc = bitcoindNetworkSumbitTx bitcoindRPCConf

    let confPubKey = HC.derivePubKey $ confSettlePrivKey cfgSettleConfig
    let confOpenPrice = if addSettleFee then basePrice + settleFee else basePrice

    liftIO . putStrLn $ "Server PubKey: " ++ cs (pathParamEncode confPubKey)

    let basePathVersion = "/v1"
    addRoutes $ mainRoutes basePathVersion

    return $ App chanOpenMap cfgSettleConfig confPubKey confOpenPrice minConf basePathVersion pushTxFunc