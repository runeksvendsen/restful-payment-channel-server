{-# LANGUAGE OverloadedStrings #-}

module Server.Init where



import           Server.App  (mainRoutes)

import           Server.Config
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
import           Snap


appInit :: ChanMapConn -> SnapletInit App App
appInit chanOpenMap = makeSnaplet "PayChanServer" "RESTful Bitcoin payment channel server" Nothing $ do
    --- CONFIG ---
    cfg <- getSnapletUserConfig

    bitcoinNetwork <- liftIO (configLookupOrFail cfg "bitcoin.network")
    liftIO $ setBitcoinNetwork bitcoinNetwork

    cfgSettleConfig@(SettleConfig _ _ settleFee _) <- SettleConfig <$>
            liftIO (configLookupOrFail cfg "settlement.privKeySeed") <*>
            liftIO (configLookupOrFail cfg "settlement.fundsDestinationAddress") <*>
            fmap calcSettlementFeeSPB (liftIO (configLookupOrFail cfg "settlement.txFeeSatoshiPerByte")) <*>
            liftIO (configLookupOrFail cfg "settlement.settlementPeriodHours")

    (OpenConfig minConf basePrice addSettleFee) <- OpenConfig <$>
            liftIO (configLookupOrFail cfg "open.fundingTxMinConf") <*>
            liftIO (configLookupOrFail cfg "open.basePrice") <*>
            liftIO (configLookupOrFail cfg "open.priceAddSettlementFee")

    bitcoindRPCConf <- BTCRPCInfo <$>
            liftIO (configLookupOrFail cfg "bitcoin.bitcoindRPC.ip") <*>
            liftIO (configLookupOrFail cfg "bitcoin.bitcoindRPC.port") <*>
            liftIO (configLookupOrFail cfg "bitcoin.bitcoindRPC.user") <*>
            liftIO (configLookupOrFail cfg "bitcoin.bitcoindRPC.pass")
    let pushTxFunc = bitcoindNetworkSumbitTx bitcoindRPCConf

    let confPubKey = HC.derivePubKey $ confSettlePrivKey cfgSettleConfig
    let confOpenPrice = if addSettleFee then basePrice + settleFee else basePrice

    liftIO . putStrLn $ "Server PubKey: " ++ cs (pathParamEncode confPubKey)

    let basePathVersion = "/v1"
    addRoutes $ mainRoutes basePathVersion

    return $ App chanOpenMap cfgSettleConfig confPubKey confOpenPrice minConf basePathVersion pushTxFunc