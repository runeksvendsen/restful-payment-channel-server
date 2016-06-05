{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.App where

-- import Paths_bitcoin_payment_channel_example (getDataDir)
import Server.Util (getPathArg, getQueryArg, getOptionalQueryArg,
                    ChanOpenConfig(..), ChanPayConfig(..),
                    bodyJSONGetPayment,
                    txInfoFromAddr, guardIsConfirmed, fundingAddrFromParams)
import Server.Config (pubKeyServer, prvKeyServer)
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
                ,   method PUT    channelPaymentHandler -- ?(change_address)
                <|> method DELETE channelDeleteHandler


            -- Hack
                <|> method OPTIONS applyCORS')
            , ("/channels/new" -- ?client_pubkey&exp_time&change_address
                ,   method OPTIONS   applyCORS')

              ]
    wrapCORS
    return $ App chanOpenMap

-- fundingInfoHandler :: MonadSnap m => m ()
fundingInfoHandler :: Handler App App ()
fundingInfoHandler =
    logFundingInfo >>
    mkFundingInfo pubKeyServer <$>
    getQueryArg "client_pubkey" <*>
    getQueryArg "exp_time"
        >>= writeFundingInfoResp

-- ChannelMap TxInfo HC.PubKey HC.Address BitcoinLockTime Payment
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

channelPaymentHandler :: Handler App App ()
channelPaymentHandler = applyCORS' >>
    PayConfig <$>
        use channelStateMap <*>
        getPathArg "funding_txid" <*>
        getPathArg "funding_vout" <*>
        getOptionalQueryArg "change_address" <*>
        bodyJSONGetPayment
    >>= chanPay


channelDeleteHandler :: Handler App App ()
channelDeleteHandler = error "STUB"



---server config---

