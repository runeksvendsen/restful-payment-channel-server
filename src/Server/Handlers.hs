{-# LANGUAGE OverloadedStrings #-}

module Server.Handlers where

import           Prelude hiding (userError)

import           Server.Types (ChanOpenConfig(..),ChanPayConfig(..),
                                StdConfig(..))
import           Server.Config.Types (App, pubKey, fundingMinConf)

import           Common.Common
import           Common.Types

import           Server.Util
import           Server.DB (tryDBRequest, trySettlementRequest, getChannelStateOr404, confirmChannelDoesntExistOrAbort)
import           BlockchainAPI.Impl.ChainSo (chainSoAddressInfo, toEither)
import           BlockchainAPI.Types (toFundingTxInfo,
                                TxInfo(..), OutInfo(..))

import qualified Server.ChanStore.Interface as DBConn

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless, when)


import           Snap.Core

import           Snap (Handler)

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft, usesBlockHeight)
import           Data.Bitcoin.PaymentChannel.Util (setSenderChangeAddress, BitcoinLockTime)

import qualified Network.Haskoin.Constants as HCC
import           Control.Lens (use)

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT

import           Data.Aeson (toJSON)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Text.Printf (printf)
import Data.String.Conversions (cs)
import Test.GenData (deriveMockFundingInfo, convertMockFundingInfo)



type URL = String

writeFundingInfoResp :: MonadSnap m => (FundingInfo, URL) -> m ()
writeFundingInfoResp (fi, url) = do
    applyCORS'
    modifyResponse $ setHeader "Location" (cs url)
    writeJSON . toJSON $ fi

mkFundingInfo ::
    BitcoinAmount ->
    Int ->
    Int ->
    HC.PubKey ->
    HC.PubKey ->
    BitcoinLockTime ->
    String ->
    (FundingInfo,URL)
mkFundingInfo openPrice' minConf settleHours recvPK sendPK lockTime rootURL =
    (FundingInfo
        recvPK
        (getFundingAddress' sendPK recvPK lockTime)
        openPrice'
        minConf
        settleHours,
    cs $ rootURL ++ channelOpenPath sendPK lockTime)



chanSettle :: MonadSnap m =>
    StdConfig
    -> (ReceiverPaymentChannel -> IO (Either String HT.TxHash))
    -> BitcoinAmount
    -> m ()
chanSettle (StdConfig chanMap chanId payment) settleChannel valRecvd = do
    chanState <- getChannelStateOr404 chanMap chanId
    -- verify payment is the most recent payment received
    case recvPayment chanState payment of
        Left _ -> userError "Invalid payment. Please provide most recent channel payment."
        Right (val, _) -> unless (val == 0) $
                userError "Invalid payment. Cannot send value in settlement request."
    -- settle channel
    settlementTxId <- either internalError return =<< liftIO (settleChannel chanState)
    -- mark channel as closed in channel store
    tryDBRequest $ DBConn.chanDelete chanMap chanId settlementTxId
    --write response
    writeJSON . toJSON $ PaymentResult {
            paymentResultchannel_status = ChannelClosed,
            paymentResultchannel_value_left = channelValueLeft chanState,
            -- If we got here because a channel payment exhausted the channel,
            --  this contains the value of the exhausting payment.
            paymentResultvalue_received = valRecvd,
            paymentResultsettlement_txid = Just settlementTxId
        }
    modifyResponse $ setResponseStatus 202 "Channel closed"



chanPay :: MonadSnap m => ChanPayConfig -> m (BitcoinAmount, ReceiverPaymentChannel)
chanPay (PayConfig (StdConfig chanMap chanId payment) maybeNewAddr) = do
    existingChanState <- getChannelStateOr404 chanMap chanId
    (valRecvd,newChanState) <- either
            (userError . show)
            return
            (recvPayment existingChanState payment)
    -- TODO: chanState can have changed to Settled or SettlementInProgress inbetween
    --  fetching and updating
    tryDBRequest $ DBConn.chanUpdate chanMap chanId payment

    modifyResponse $ setResponseStatus 200 (C.pack "Payment accepted")
    return (valRecvd,newChanState)


channelOpenHandler :: MonadSnap m =>
    ChanOpenConfig
    -> m (BitcoinAmount, ReceiverPaymentChannel)
channelOpenHandler
    (ChanOpenConfig openPrice pubKeyServ chanMap
        txInfo@(TxInfo txId _ (OutInfo _ _ idx))
        basePath sendPK sendChgAddr lockTime payment) =
    do
        when (usesBlockHeight lockTime) $
            userError "Block number as channel expiration date is unsupported"

        let chanId = HT.OutPoint txId (fromIntegral idx)
        confirmChannelDoesntExistOrAbort chanMap basePath chanId

        (valRecvd,recvChanState) <- either (userError . show) return $
            channelFromInitialPayment
                (CChannelParameters sendPK pubKeyServ lockTime)
                (toFundingTxInfo txInfo) sendChgAddr payment

        when (valRecvd < openPrice) $
            userError $ "Initial payment short. Channel open price is " ++
                show openPrice ++ ", received " ++ show valRecvd ++ "."

        tryDBRequest $ DBConn.chanAdd chanMap recvChanState
        modifyResponse $ setResponseStatus 201 (C.pack "Channel ready")

        unless (channelIsExhausted recvChanState) $
            httpLocationSetActiveChannel basePath chanId

        return (valRecvd,recvChanState)





