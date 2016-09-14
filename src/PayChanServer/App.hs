{-# LANGUAGE OverloadedStrings #-}

module  PayChanServer.App where


import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount)

import           Common.Util (getPathArg, getQueryArg, getOptionalQueryArg, applyCORS')
import           PayChanServer.Util
import           PayChanServer.Config.Types
import           PayChanServer.Types ( OpenHandlerConf(..),ChanPayConfig(..),
                                StdConfig(..), ServerSettleConfig(..))
import           PayChanServer.Handlers
import qualified BlockchainAPI.Impl.Bitcoind.Interface as Btc

import           Control.Applicative ((<|>))
import           Control.Lens (use)

import qualified Data.ByteString as BS
import           Snap
import           Data.Monoid ((<>))



mainRoutes :: BS.ByteString -> [(BS.ByteString, Handler App App ())]
mainRoutes basePath' =
    [
     (basePath' <> "/fundingInfo" -- ?client_pubkey&exp_time
       ,   method GET  fundingInfoHandler)

   , (basePath' <> "/channels/new" -- ?client_pubkey&exp_time&change_address
       ,   method POST (newChannelHandler >>= writePaymentResult >>=
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
    getServerPubKey <*>
    getClientPubKey <*>
    getQueryArg "exp_time" <*>
    (use basePath >>= getAppRootURL)
        >>= writeFundingInfoResp

newChannelHandler :: Handler App App (BitcoinAmount, ReceiverPaymentChannel)
newChannelHandler = applyCORS' >>
    OpenHandlerConf <$>
        use finalOpenPrice <*>
        getServerPubKey <*>
        use dbInterface <*>
        blockchainGetFundingInfo <*>
        getClientPubKey <*>
        getQueryArg "change_address" <*>
        (getQueryArg "exp_time" >>= checkExpirationTime) <*>
        getQueryArg "payment"
    >>= channelOpenHandler

paymentHandler :: Handler App App (BitcoinAmount, ReceiverPaymentChannel)
paymentHandler = applyCORS' >>
    PayConfig <$>
        getActiveChanConf <*>
        getOptionalQueryArg "change_address"
    >>= chanPay

settlementHandler :: BitcoinAmount -> Handler App App ()
settlementHandler valueReceived = do
    applyCORS'

    settleChanFunc <- use settleChannel
    stdConf <- getActiveChanConf
    chanSettle stdConf settleChanFunc valueReceived

