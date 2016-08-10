{-# LANGUAGE OverloadedStrings #-}

module  PayChanServer.Handlers where


import           Snap.Core
import           Snap (Handler)

import           Prelude hiding (userError)

import           PayChanServer.Types (OpenHandlerConf(..),ChanPayConfig(..),
                                StdConfig(..))
import           PayChanServer.Config.Types (App, pubKey, openMinConf, openConfig)
import           PayChanServer.Util

import           Common.Types
import           Common.ResourceURL (channelOpenPath)

import           Common.Util
import           PayChanServer.DB (tryDBRequest, trySigningRequest,
                            getChannelStateForPayment, getChannelStateForSettlement,
                            confirmChannelDoesntExistOrAbort, getChannelStateOr404)

import qualified ChanStore.Interface as DBConn
import           ChanStore.Lib.Types (UpdateResult(..))

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless, when)


import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    FundingTxInfo(CFundingTxInfo),
                                                    SendPubKey(..), RecvPubKey(..), IsPubKey(getPubKey),
                                                    channelValueLeft, usesBlockHeight)
import           Data.Bitcoin.PaymentChannel.Util (setSenderChangeAddress, BitcoinLockTime(..))

import qualified Network.Haskoin.Transaction as HT

import           Data.Aeson (toJSON)

import qualified Data.ByteString.Char8 as C
import Data.String.Conversions (cs)



type URL = String

writeFundingInfoResp :: MonadSnap m => (FundingInfo, URL) -> m ()
writeFundingInfoResp (fi, url) = do
    applyCORS'
    modifyResponse $ setHeader "Location" (cs url)
    writeJSON . toJSON $ fi

mkFundingInfo ::
    BitcoinAmount ->
    Word ->
    Word ->
    RecvPubKey ->
    SendPubKey ->
    BitcoinLockTime ->
    String ->
    (FundingInfo,URL)
mkFundingInfo openPrice' minConf settleHours recvPK sendPK lockTime rootURL =
    (FundingInfo
        recvPK
        (getFundingAddress' sendPK recvPK lockTime)
        openPrice'
        (fromIntegral minConf)
        (fromIntegral settleHours),
    cs $ rootURL ++ channelOpenPath sendPK lockTime)



chanSettle :: MonadSnap m =>
    StdConfig
    -> (ReceiverPaymentChannel -> IO HT.TxHash)
    -> BitcoinAmount
    -> m ()
chanSettle (StdConfig chanMap chanId clientPayment) settleChannel valRecvd = do
    chanState <- getChannelStateOr404 chanMap chanId
    -- Authenticate. Most recent channel payment is supplied by client as a token.
    let confirmClientPayment storedPayment =
            unless (clientPayment == storedPayment) $
                userError "Invalid payment. Please provide most recent channel payment."
    -- Decide what to do based on channel state: open/half-open/closed
    (settlementTxId,chanValLeft) <- case chanState of
            (DBConn.ReadyForPayment rpc) -> do
                confirmClientPayment $ getNewestPayment rpc
                settleTxId <- liftIO (settleChannel rpc)
                return (settleTxId, channelValueLeft rpc)
            (DBConn.SettlementInProgress _) ->
                errorWithDescription 410 "Channel is being closed"
            (DBConn.ChannelSettled settleTxId _ rpc) -> do
                confirmClientPayment $ getNewestPayment rpc
                return (settleTxId, channelValueLeft rpc)

    --write response
    writeJSON . toJSON $ PaymentResult {
            paymentResultchannel_status = ChannelClosed,
            paymentResultchannel_value_left = chanValLeft,
            -- If we got here because a channel payment exhausted the channel,
            --  this contains the value of the exhausting payment.
            paymentResultvalue_received = valRecvd,
            paymentResultsettlement_txid = Just settlementTxId
        }
    modifyResponse $ setResponseStatus 200 "Channel closed"



chanPay :: MonadSnap m => ChanPayConfig -> m (BitcoinAmount, ReceiverPaymentChannel)
chanPay (PayConfig (StdConfig dbConn chanId payment) _) = do
    existingChanState <- getChannelStateForPayment dbConn chanId
    (valRecvd,newChanState) <- either
            (userError . show)
            return
            (recvPayment existingChanState payment)
    updateResult <- tryDBRequest $ DBConn.chanUpdate dbConn chanId payment
    case updateResult of
        WasUpdated -> do
            modifyResponse $ setResponseStatus 200 (C.pack "Payment accepted")
            return (valRecvd,newChanState)
        WasNotUpdated ->    -- Channel no longer open
            errorWithDescription 410 "Channel closed or being closed"


channelOpenHandler ::
    OpenHandlerConf
    -> Handler App App (BitcoinAmount, ReceiverPaymentChannel)
channelOpenHandler
    (OpenHandlerConf openPrice pubKeyServ chanMap fundingTxInfo@(CFundingTxInfo fundTxId fundIdx _)
    sendPK sendChgAddr lockTime payment) = do
        let chanId = HT.OutPoint fundTxId fundIdx

        confirmChannelDoesntExistOrAbort chanMap chanId

        (valRecvd,recvChanState) <- either (userError . show) return $
            channelFromInitialPayment
                (CChannelParameters sendPK pubKeyServ lockTime)
                fundingTxInfo sendChgAddr payment

        when (valRecvd < openPrice) $
            userError $ "Initial payment short: open price is " ++
                show openPrice ++ ", received " ++ show valRecvd

        tryDBRequest $ DBConn.chanAdd chanMap recvChanState

        modifyResponse $ setResponseStatus 201 (C.pack "Channel ready")
        unless (channelIsExhausted recvChanState) $
            httpLocationSetActiveChannel chanId

        return (valRecvd,recvChanState)





