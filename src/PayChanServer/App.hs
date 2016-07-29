{-# LANGUAGE OverloadedStrings #-}

module  PayChanServer.App where


import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount)

import           PayChanServer.Util (getPathArg, getQueryArg, getOptionalQueryArg, getAppRootURL,
                              channelIDFromPathArgs, writePaymentResult, proceedIfExhausted,
                              blockchainGetFundingInfo, checkExpirationTime,
                              applyCORS')
import           PayChanServer.Util
import           PayChanServer.Config.Types
import           PayChanServer.Types ( OpenHandlerConf(..),ChanPayConfig(..),
                                StdConfig(..), ServerSettleConfig(..))
import           PayChanServer.Handlers

import           Control.Applicative ((<|>))
import           Control.Lens (use)

import qualified Data.ByteString as BS
import           Snap
import           Data.Monoid ((<>))

type Debug = Bool

mainRoutes :: Debug -> BS.ByteString -> [(BS.ByteString, Handler App App ())]
mainRoutes debug basePath' =
    let
        -- Testing: Bypass Blockchain lookup/submit in case we're debugging
        handleChannelOpen       = newChannelHandler debug
    in
        [
         (basePath' <> "/fundingInfo" -- ?client_pubkey&exp_time
           ,   method GET    fundingInfoHandler)

       , (basePath' <> "/channels/new" -- ?client_pubkey&exp_time&change_address
           ,   method POST (handleChannelOpen >>= writePaymentResult >>=
                               proceedIfExhausted >>= settlementHandler)
               <|> method OPTIONS applyCORS') --CORS

       , (basePath' <> "/channels/:funding_txid/:funding_vout"
           ,   method PUT    (paymentHandler >>= writePaymentResult >>=
                               proceedIfExhausted >>= settlementHandler)
           <|> method DELETE (settlementHandler 0)
           <|> method OPTIONS applyCORS') --CORS
        ] :: [(BS.ByteString, Handler App App ())]


fundingInfoHandler :: Handler App App ()
fundingInfoHandler =
    mkFundingInfo <$>
    use finalOpenPrice <*>
    (openMinConf <$> use openConfig) <*>
    use settlePeriod <*>
    use pubKey <*>
    getQueryArg "client_pubkey" <*>
    getQueryArg "exp_time" <*>
    (use basePath >>= getAppRootURL)
        >>= writeFundingInfoResp

newChannelHandler :: Debug -> Handler App App (BitcoinAmount, ReceiverPaymentChannel)
newChannelHandler debug = applyCORS' >>
    OpenHandlerConf <$>
        use finalOpenPrice <*>
        use pubKey <*>
        use dbConn <*>
        blockchainGetFundingInfo debug <*>
        use basePath <*>
        getQueryArg "client_pubkey" <*>
        getQueryArg "change_address" <*>
        (getQueryArg "exp_time" >>= checkExpirationTime) <*>
        getQueryArg "payment"
    >>= channelOpenHandler

paymentHandler :: Handler App App (BitcoinAmount, ReceiverPaymentChannel)
paymentHandler = applyCORS' >>
    PayConfig <$>
        (StdConfig <$>
            use dbConn <*>
            channelIDFromPathArgs <*>
            getQueryArg "payment") <*>
        getOptionalQueryArg "change_address"
    >>= chanPay

settlementHandler :: BitcoinAmount -> Handler App App ()
settlementHandler valueReceived = do
    applyCORS'

    settleChanFunc <- use settleChanFunc
    stdConf <- StdConfig <$>
            use dbConn <*>
            channelIDFromPathArgs <*>
            getQueryArg "payment"
    chanSettle stdConf settleChanFunc valueReceived

