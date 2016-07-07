{-# LANGUAGE OverloadedStrings #-}

module Server.Handlers where

import           Prelude hiding (userError)

import           Server.Types (ChanOpenConfig(..),ChanPayConfig(..),
                                StdConfig(..), ChanSettleConfig(..))
import           Server.Config (App(..), pubKey, fundingMinConf)

import           Common.Common
import           Common.Types

import           Server.Util
import           BlockchainAPI.Impl.ChainSo (chainSoAddressInfo, toEither)
import           BlockchainAPI.Types (toFundingTxInfo,
                                TxInfo(..), OutInfo(..))
-- import           Server.ChanStore (ChannelMap, ChanState(..),
--                                    addChanState, updateChanState, deleteChanState, isSettled)
import qualified Server.ChanStore.Client as DBConn
import           Server.ChanStore.Settlement (settleChannel)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless, when)


import           Snap.Core

import           Snap (Handler)

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft)
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



--- /fundingInfo ---
type URL = String

writeFundingInfoResp :: MonadSnap m => (FundingInfo,URL) -> m ()
writeFundingInfoResp (fi, url) = do
    applyCORS'
    modifyResponse $ setHeader "Location" (cs url)
    writeJSON . toJSON $ fi
    writeBS "\n"

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

logFundingInfo :: MonadSnap m => m ()
logFundingInfo  = do
    pk <- getQueryArg "client_pubkey"
    expTime <- getQueryArg "exp_time"
    liftIO . putStrLn $ printf
        "Got fundingInfo request: pubkey=%s exp=%s"
            (show (pk :: HC.PubKey))
            (show (expTime :: BitcoinLockTime))

--- /fundingInfo ---


----Settlement----
chanSettle :: MonadSnap m =>
    ChanSettleConfig
    -> StdConfig
    -> (HT.Tx -> IO (Either String HT.TxHash))
    -> BitcoinAmount
    -> m ()
chanSettle (SettleConfig privKey recvAddr txFee _)
    (StdConfig chanMap hash _ payment) pushTx valRecvd = do
    chanState <- getChannelStateOr404 chanMap hash

    -- verify payment is the most recent payment received
    case recvPayment chanState payment of
        Left _ -> userError "Invalid payment. Please provide most recent channel payment."
        Right (val, _) -> unless (val == 0) $
                userError "Invalid payment. Cannot send value in delete request."

    settlementTxId <- either internalError return =<<
            liftIO (settleChannel privKey recvAddr txFee pushTx chanState)

    -- mark channel as closed in channel store
    exists <- liftIO $ DBConn.chanDelete chanMap hash settlementTxId
    when (exists == False) $
        logError "Tried to delete channel map item that doesn't exist"

    writeJSON . toJSON $ PaymentResult {
            paymentResultchannel_status = ChannelClosed,
            paymentResultchannel_value_left = channelValueLeft chanState,
            -- If we got here because a channel payment exhausted the channel,
            --  this contains the value of the exhausting payment.
            paymentResultvalue_received = valRecvd,
            paymentResultsettlement_txid = Just settlementTxId
        }

    modifyResponse $ setResponseStatus 202 "Channel closed"
---


----Payment----

chanPay :: MonadSnap m => ChanPayConfig -> m (BitcoinAmount, ReceiverPaymentChannel)
chanPay (PayConfig chanMap hash _ maybeNewAddr payment) = do
    existingChanState <- maybeUpdateChangeAddress maybeNewAddr =<<
            getChannelStateOr404 chanMap hash
    (valRecvd,newChanState) <- either
            (userError . show)
            return
            (recvPayment existingChanState payment)

    exists <- liftIO $ DBConn.chanUpdate chanMap hash newChanState
    when (exists == False) $
        logError "Tried to update channel map item that doesn't exist"

    modifyResponse $ setResponseStatus 200 (C.pack "Payment accepted")
    return (valRecvd,newChanState)

----Payment----


--- POST /channels/ ----
channelOpenHandler :: MonadSnap m =>
    ChanOpenConfig
    -> m (BitcoinAmount, ReceiverPaymentChannel)
channelOpenHandler
    (ChanOpenConfig openPrice pubKeyServ chanMap
        txInfo@(TxInfo txId _ (OutInfo _ chanVal idx))
        basePath sendPK sendChgAddr lockTime payment) =
    do
        liftIO . putStrLn $ "Processing channel open request... " ++
            show (sendPK, lockTime, txInfo, payment)

        confirmChannelDoesntExistOrAbort chanMap basePath txId idx

        (valRecvd,recvChanState) <- either (userError . show) return $
            channelFromInitialPayment
                (CChannelParameters sendPK pubKeyServ lockTime)
                (toFundingTxInfo txInfo) sendChgAddr payment

        when (valRecvd < openPrice) $
            userError $ "Initial payment short. Channel open price is " ++
                show openPrice ++ ", received " ++ show valRecvd ++ "."

        liftIO $ DBConn.chanAdd chanMap txId recvChanState
        modifyResponse $ setResponseStatus 201 (C.pack "Channel ready")

        unless (channelIsExhausted recvChanState) $
            httpLocationSetActiveChannel basePath txId idx

        return (valRecvd,recvChanState)





--- Util ---
proceedIfExhausted :: MonadSnap m => (ChannelStatus,BitcoinAmount) -> m BitcoinAmount
proceedIfExhausted (ChannelOpen,_)          = finishWith =<< getResponse
proceedIfExhausted (ChannelClosed,valRecvd) = return valRecvd

applyCORS' :: MonadSnap m => m ()
applyCORS' = do
    modifyResponse $ setHeader "Access-Control-Allow-Origin"    "*"
    modifyResponse $ setHeader "Access-Control-Allow-Methods"   "GET,POST,PUT,DELETE"
    modifyResponse $ setHeader "Access-Control-Allow-Headers"   "Origin,Accept,Content-Type"
    modifyResponse $ setHeader "Access-Control-Expose-Headers"  "Location"

writePaymentResult :: MonadSnap m =>
    (BitcoinAmount, ReceiverPaymentChannel)
    -> m (ChannelStatus,BitcoinAmount)
writePaymentResult (valRecvd,recvChanState) =
    let chanStatus =
            if channelIsExhausted recvChanState then
                ChannelClosed else
                ChannelOpen
    in
        do
            writeJSON . toJSON $ PaymentResult {
                paymentResultchannel_status = chanStatus,
                paymentResultchannel_value_left = channelValueLeft recvChanState,
                paymentResultvalue_received = valRecvd,
                paymentResultsettlement_txid = Nothing
            }
            return (chanStatus,valRecvd)

confirmChannelDoesntExistOrAbort :: MonadSnap m => DBConn.ChanMapConn -> BS.ByteString -> HT.TxHash -> Integer -> m ()
confirmChannelDoesntExistOrAbort chanMap basePath hash idx = do
    maybeItem <- liftIO (DBConn.chanGet chanMap hash)
    case fmap DBConn.isSettled maybeItem of
        Nothing -> return ()    -- channel doesn't already exist
        Just False ->           -- channel exists already, and is open
            httpLocationSetActiveChannel basePath hash idx >>
            errorWithDescription 409 "Channel already exists"
        Just True  ->           -- channel in question has been settled
            errorWithDescription 409 "Channel already existed, but has been settled"


getChannelStateOr404 :: MonadSnap m => DBConn.ChanMapConn -> HT.TxHash -> m ReceiverPaymentChannel
getChannelStateOr404 chanMap hash =
    liftIO (DBConn.chanGet chanMap hash) >>=
    maybe
        (errorWithDescription 404 "No such channel")
        (return . DBConn.csState)

maybeUpdateChangeAddress :: MonadSnap m =>
    Maybe HC.Address
    -> ReceiverPaymentChannel
    -> m ReceiverPaymentChannel
maybeUpdateChangeAddress maybeAddr state =
    maybe (return state) updateAddressAndLog maybeAddr
        where updateAddressAndLog addr = do
                liftIO . putStrLn $ "Updating client change address to " ++ toString addr
                return $ setSenderChangeAddress state addr
--- Util ---


--- Funding ---
tEST_blockchainGetFundingInfo :: Handler App App TxInfo
tEST_blockchainGetFundingInfo = do
    pubKeyServer <- use pubKey
    minConf <- use fundingMinConf
    testArgTrue <- fmap (== Just True) $ getOptionalQueryArg "test"

    if (HCC.getNetworkName HCC.getNetwork == "testnet") && testArgTrue then
            test_GetDerivedFundingInfo pubKeyServer
        else
            fundingAddressFromParams pubKeyServer >>=
                blockchainAddressCheckEverything minConf

blockchainAddressCheckEverything :: MonadSnap m => Int -> HC.Address -> m TxInfo
blockchainAddressCheckEverything minConf addr =
    (liftIO . chainSoAddressInfo . cs . HC.addrToBase58) addr >>=
        either internalError return . toEither >>=
        maybe
            (userError $ "No transactions paying to " ++ cs (HC.addrToBase58 addr) ++
                ". Maybe wait a little?")
            return >>=
        guardIsConfirmed (fromIntegral minConf)

-- | Deterministically derives a mock TxInfo from ChannelParameters,
-- which matches that of the test data generated by Test.GenData.
test_GetDerivedFundingInfo :: MonadSnap m => HC.PubKey -> m TxInfo
test_GetDerivedFundingInfo pubKeyServer = do
    cp <- flip CChannelParameters pubKeyServer <$>
            getQueryArg "client_pubkey" <*>
            getQueryArg "exp_time"
    return $ convertMockFundingInfo . deriveMockFundingInfo $ cp
--- Funding ---
