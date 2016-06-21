{-# LANGUAGE OverloadedStrings #-}

module Server.App where


import           Data.Bitcoin.PaymentChannel.Types (PaymentChannel(channelIsExhausted),
                                                    ReceiverPaymentChannel, BitcoinAmount)

import           Server.Util (getPathArg, getQueryArg, getOptionalQueryArg,
                                ChanOpenConfig(..), ChanPayConfig(..),
                                StdConfig(..), headerGetPayment,
                                txInfoFromAddr)
import           Server.Config -- (calcSettlementFeeSPB, App(..))
import           Server.Types (OpenConfig(..), ChanSettleConfig(..))
import           Server.Handlers -- (mkFundingInfo, writeFundingInfoResp, channelOpenHandler)

import           Common.Common (pathParamEncode)
import           Data.String.Conversions (cs)

import           Server.ChanStore (ChannelMap,
                                   newChanMap, diskSyncThread)

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (forkIO)
import           Control.Lens.TH
import           Control.Lens (use)

import           Data.Configurator (require)
import qualified Network.Haskoin.Crypto as HC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap
import           Snap.CORS
import           Snap.Util.FileServe (serveDirectory)


mainRoutes basePath =
    let
        mainRoutes = [
             (basePath `mappend` "/fundingInfo" -- ?client_pubkey&exp_time
               ,   method GET    fundingInfoHandler)

           , (basePath `mappend` "/channels/new" -- ?client_pubkey&exp_time&change_address
               ,   method POST (newChannelHandler >>= writePaymentResult >>=
                                   proceedIfExhausted >> settlementHandler)
                   <|> method OPTIONS applyCORS') --CORS

           , (basePath `mappend` "/channels/:funding_txid/:funding_vout"
               ,   method PUT    (paymentHandler >>= writePaymentResult >>=
                                   proceedIfExhausted >> settlementHandler)
               <|> method DELETE settlementHandler
               <|> method OPTIONS applyCORS') --CORS
            ] :: [(BS.ByteString, Handler App App ())]


        docRoute = [ ("/", serveDirectory "dist") ] :: [(BS.ByteString, Handler b v ())]

    in  mainRoutes ++ docRoute

appInit :: SnapletInit App App
appInit = makeSnaplet "PayChanServer" "Payment channel REST interface" Nothing $ do
    cfg <- getSnapletUserConfig     -- devel.cfg

    bitcoinNetwork <- liftIO (require cfg "bitcoin.network")
    liftIO $ setBitcoinNetwork bitcoinNetwork
    let basePath = "/v1/" `mappend` toPathString bitcoinNetwork

    settleConfig@(SettleConfig _ _ settleFee _) <- SettleConfig <$>
            liftIO (require cfg "settlement.privKeySeed") <*>
            liftIO (require cfg "settlement.fundsDestinationAddress") <*>
            fmap calcSettlementFeeSPB (liftIO (require cfg "settlement.txFeeSatoshiPerByte")) <*>
            liftIO (require cfg "settlement.settlementPeriodHours")

    (OpenConfig minConf basePrice addSettleFee) <- OpenConfig <$>
            liftIO (require cfg "open.fundingTxMinConf") <*>
            liftIO (require cfg "open.basePrice") <*>
            liftIO (require cfg "open.priceAddSettlementFee")

    let pubKey = HC.derivePubKey $ confSettlePrivKey settleConfig
    let openPrice = if addSettleFee then basePrice + settleFee else basePrice

    liftIO . putStrLn $ "Channel PubKey: " ++ cs (pathParamEncode pubKey)

    hostname <- liftIO (require cfg "network.hostname")

    -- Disk channel store setup (TODO: fix)
    chanOpenMap <- liftIO newChanMap
    liftIO . forkIO $ diskSyncThread chanOpenMap 5

    addRoutes $ mainRoutes basePath

    return $ App chanOpenMap settleConfig pubKey openPrice minConf basePath hostname


fundingInfoHandler :: Handler App App ()
fundingInfoHandler =
    mkFundingInfo <$>
    use openPrice <*>
    use fundingMinConf <*>
    fmap confSettlePeriod (use settleConfig) <*>
    use pubKey <*>
    getQueryArg "client_pubkey" <*>
    getQueryArg "exp_time" <*>
    use hostname <*>
    use basePath
        >>= writeFundingInfoResp


newChannelHandler :: Handler App App (BitcoinAmount, ReceiverPaymentChannel)
newChannelHandler = applyCORS' >>
    ChanOpenConfig <$>
        use openPrice <*>
        use pubKey <*>
        use channelStateMap <*>
        tEST_blockchainGetFundingInfo <*>
        use hostname <*>
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
    stdConf <- StdConfig <$>
            use channelStateMap <*>
            getPathArg "funding_txid" <*>
            getPathArg "funding_vout" <*>
            getQueryArg "payment"
    chanSettle settleConf stdConf

