{-# LANGUAGE OverloadedStrings #-}

module Server.Init where



import           Server.App  (mainRoutes)

import           Server.Config
import           Server.Types (OpenConfig(..), ChanSettleConfig(..))

import           Bitcoind (BTCRPCInfo(..), bitcoindNetworkSumbitTx)

import           Common.Common (pathParamEncode)
import           Data.String.Conversions (cs)

import           Server.ChanStore ( -- ChannelMap,
                                   newChanMap, diskSyncThread, diskSyncNow, mapLen)

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (forkIO, killThread,     threadDelay)
import qualified System.Posix.Signals as Sig (Handler(Catch), installHandler, sigTERM)

import qualified Network.Haskoin.Crypto as HC
import           Snap


appInit :: SnapletInit App App
appInit = makeSnaplet "PayChanServer" "RESTful Bitcoin payment channel server" Nothing $ do
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

    -- Disk channel store setup
    stateBaseDir <- liftIO (configLookupOrFail cfg "storage.stateDir")
    chanOpenMap <- liftIO $ newChanMap stateBaseDir
    chanMapLen <- liftIO $ mapLen chanOpenMap
    unless (chanMapLen == 0) $
        liftIO $ putStrLn $ "Restored " ++ show chanMapLen ++ " open channel states"
    tid <- liftIO . forkIO $ diskSyncThread chanOpenMap 5
    -- If we receive a TERM signal, kill the sync thread & sync immediately
    _ <- liftIO $ Sig.installHandler
        Sig.sigTERM
        (Sig.Catch $
            killThread tid >>
            putStrLn "Received TERM signal, syncing channel map to disk before shutting down." >>
            diskSyncNow chanOpenMap
        )
        Nothing

    let basePathVersion = "/v1"
    addRoutes $ mainRoutes basePathVersion

    return $ App chanOpenMap cfgSettleConfig confPubKey confOpenPrice minConf basePathVersion pushTxFunc