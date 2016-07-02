{-# LANGUAGE OverloadedStrings #-}

module Server.App where


import           Data.Bitcoin.PaymentChannel.Types (PaymentChannel(channelIsExhausted),
                                                    ReceiverPaymentChannel, BitcoinAmount)

import           Server.Util (getPathArg, getQueryArg, getOptionalQueryArg, getAppRootURL)
import           Server.Config
import           Server.Types (OpenConfig(..),
                                ChanOpenConfig(..),ChanPayConfig(..),
                                StdConfig(..), ChanSettleConfig(..))
import           Server.Handlers

import           Bitcoind (BTCRPCInfo(..), bitcoindNetworkSumbitTx)

import           Common.Common (pathParamEncode)
import           Data.String.Conversions (cs)

import           Server.ChanStore (ChannelMap,
                                   newChanMap, diskSyncThread, diskSyncNow)

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (forkIO, killThread,     threadDelay)
import           Control.Lens.TH
import           Control.Lens (use)
import qualified System.Posix.Signals as Sig (Handler(Catch), installHandler, sigTERM)

-- import           Data.Configurator (require, lookup)
import qualified Network.Haskoin.Crypto as HC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap
-- import           Snap.CORS
import           Snap.Util.FileServe (serveDirectory)
import           Data.Monoid ((<>))


mainRoutes basePath =
        [
         (basePath <> "/fundingInfo" -- ?client_pubkey&exp_time
           ,   method GET    fundingInfoHandler)

       , (basePath <> "/channels/new" -- ?client_pubkey&exp_time&change_address
           ,   method POST (newChannelHandler >>= writePaymentResult >>=
                               proceedIfExhausted >> settlementHandler)
               <|> method OPTIONS applyCORS') --CORS

       , (basePath <> "/channels/:funding_txid/:funding_vout"
           ,   method PUT    (paymentHandler >>= writePaymentResult >>=
                               proceedIfExhausted >> settlementHandler)
           <|> method DELETE settlementHandler
           <|> method OPTIONS applyCORS') --CORS
        ] :: [(BS.ByteString, Handler App App ())]



appInit :: SnapletInit App App
appInit = makeSnaplet "PayChanServer" "Payment channel REST interface" Nothing $ do
    --- CONFIG ---
    cfg <- getSnapletUserConfig

    bitcoinNetwork <- liftIO (configLookupOrFail cfg "bitcoin.network")
    liftIO $ setBitcoinNetwork bitcoinNetwork
    let basePath = "/v1"  -- <> toPathString bitcoinNetwork

    settleConfig@(SettleConfig _ _ settleFee _) <- SettleConfig <$>
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

    let pubKey = HC.derivePubKey $ confSettlePrivKey settleConfig
    let openPrice = if addSettleFee then basePrice + settleFee else basePrice

    liftIO . putStrLn $ "Server PubKey: " ++ cs (pathParamEncode pubKey)

    -- Disk channel store setup
    stateBaseDir <- liftIO (configLookupOrFail cfg "storage.stateDir")
    chanOpenMap <- liftIO $ newChanMap stateBaseDir
    tid <- liftIO . forkIO $ diskSyncThread chanOpenMap 5
    -- If we receive a TERM signal, kill the sync thread & sync immediately
    liftIO $ Sig.installHandler
        Sig.sigTERM
        (Sig.Catch $ killThread tid >> diskSyncNow chanOpenMap >> threadDelay (round $ 3 * 1e6) ) --DEBUG
        Nothing

    addRoutes $ mainRoutes basePath

    return $ App chanOpenMap settleConfig pubKey openPrice minConf basePath pushTxFunc





fundingInfoHandler :: Handler App App ()
fundingInfoHandler =
    mkFundingInfo <$>
    use openPrice <*>
    use fundingMinConf <*>
    fmap confSettlePeriod (use settleConfig) <*>
    use pubKey <*>
    getQueryArg "client_pubkey" <*>
    getQueryArg "exp_time" <*>
    (use basePath >>= getAppRootURL)
        >>= writeFundingInfoResp


newChannelHandler :: Handler App App (BitcoinAmount, ReceiverPaymentChannel)
newChannelHandler = applyCORS' >>
    ChanOpenConfig <$>
        use openPrice <*>
        use pubKey <*>
        use channelStateMap <*>
        tEST_blockchainGetFundingInfo <*>
        use basePath <*>
        getQueryArg "client_pubkey" <*>
        getQueryArg "change_address" <*>
        getQueryArg "exp_time" <*>
        getQueryArg "payment"
    >>= channelOpenHandler

paymentHandler :: Handler App App (BitcoinAmount, ReceiverPaymentChannel)
paymentHandler = applyCORS' >>
    PayConfig <$>
        use channelStateMap <*>
        getPathArg "funding_txid" <*>
        getPathArg "funding_vout" <*>
        getOptionalQueryArg "change_address" <*>
        getQueryArg "payment"
    >>= chanPay

settlementHandler :: Handler App App ()
settlementHandler = do
    applyCORS'

    settleConf <- use settleConfig
    pushTxFunc <- use bitcoinPushTx
    stdConf <- StdConfig <$>
            use channelStateMap <*>
            getPathArg "funding_txid" <*>
            getPathArg "funding_vout" <*>
            getQueryArg "payment"
    chanSettle settleConf stdConf pushTxFunc

