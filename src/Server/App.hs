{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.App where

-- import Paths_bitcoin_payment_channel_example (getDataDir)
import Server.Util (getPathArg, getQueryArg, getOptionalQueryArg,
                    ChanOpenConfig(..), ChanPayConfig(..), ChanSettleConfig(..),
                    bodyJSONGetPayment,
                    txInfoFromAddr, guardIsConfirmed, fundingAddrFromParams)
import Server.Config (pubKeyServer, prvKeyServer,fundsDestAddr,settlementTxFee)
import Server.Handlers -- (mkFundingInfo, writeFundingInfoResp, channelOpenHandler)

import           Server.ChanStore (ChannelMap,
                                   newChanMap, diskSyncThread)

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (forkIO)
import           Control.Lens.TH
import           Control.Lens (use)

import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Snap
import           Snap.CORS
import           Snap.Util.FileServe (serveDirectory)


-- dataDir = fmap (++ "/resources") getDataDir

data App = App
 { _channelStateMap :: ChannelMap
 }

-- Template Haskell magic
makeLenses ''App


appInit :: SnapletInit App App
appInit = makeSnaplet "PayChanServer" "Payment channel REST interface" Nothing $ do --(Just dataDir)
    chanOpenMap <- liftIO newChanMap
    liftIO . forkIO $ diskSyncThread chanOpenMap 5
    addRoutes [
              ("/fundingInfo" -- ?client_pubkey&exp_time
                ,   method GET    fundingInfoHandler)

            , ("/channels/new" -- ?client_pubkey&exp_time&change_address
                ,   method POST   newChannelHandler)

            , ("/channels/:funding_txid/:funding_vout"
                ,   method PUT    paymentHandler -- ?(change_address)
                <|> method DELETE settlementHandler
                <|> method OPTIONS applyCORS') -- CORS

            , ("/"
                , serveDirectory "dist")

            -- CORS
            , ("/channels/new" -- ?client_pubkey&exp_time&change_address
                ,   method OPTIONS   applyCORS')
              ]
    wrapCORS
    return $ App chanOpenMap


fundingInfoHandler :: Handler App App ()
fundingInfoHandler =
    logFundingInfo >>
    mkFundingInfo pubKeyServer <$>
    getQueryArg "client_pubkey" <*>
    getQueryArg "exp_time"
        >>= writeFundingInfoResp

newChannelHandler :: Handler App App ()
newChannelHandler = applyCORS' >>
    OpenConfig <$>
        use channelStateMap <*>
        blockchainGetFundingInfo <*>
        getQueryArg "client_pubkey" <*>
        getQueryArg "change_address" <*>
        getQueryArg "exp_time" <*>
        bodyJSONGetPayment
    >>= channelOpenHandler (prvKeyServer,pubKeyServer)

paymentHandler :: Handler App App ()
paymentHandler = applyCORS' >>
    PayConfig <$>
        use channelStateMap <*>
        getPathArg "funding_txid" <*>
        getPathArg "funding_vout" <*>
        getOptionalQueryArg "change_address" <*>
        bodyJSONGetPayment
    >>= chanPay

settlementHandler :: Handler App App ()
settlementHandler = applyCORS' >>
    SettleConfig
        prvKeyServer
        fundsDestAddr
        settlementTxFee <$>
        use channelStateMap <*>
        getPathArg "funding_txid" <*>
        getPathArg "funding_vout" <*>
        bodyJSONGetPayment
    >>= chanSettle


