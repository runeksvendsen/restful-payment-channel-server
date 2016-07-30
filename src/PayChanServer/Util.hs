{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PayChanServer.Util where

import           Prelude hiding (userError)


import           Common.Types
import           Common.Util
import           Common.ResourceURL

import           BlockchainAPI.Impl.BlockrIo (txIDFromAddr, fundingOutInfoFromTxId)
import           BlockchainAPI.Types (txConfs, toFundingTxInfo,
                                TxInfo(..), OutInfo(..))

import           Snap
import           Snap.Iteratee (Enumerator, enumBuilder)

import           Data.Bitcoin.PaymentChannel.Types (PaymentChannel(..), ReceiverPaymentChannel,
                                                    ChannelParameters(..), PayChanError(..), FundingTxInfo
                                                    ,getChannelState, BitcoinAmount, Payment, BitcoinLockTime(..),
                                                    SendPubKey(..),RecvPubKey(..))
import           Data.Bitcoin.PaymentChannel.Util (deserEither, setSenderChangeAddress)

import           Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON,
                            decode, encode)
import           Text.Printf (printf)
import           Data.String.Conversions (cs)
import           Data.Aeson.Encode.Pretty (encodePretty)

import qualified Network.Haskoin.Constants as HCC
import           Control.Monad.IO.Class
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Data.Int (Int64)
import           Data.Time.Clock (UTCTime, addUTCTime)

import qualified Data.Binary as Bin (Binary, encode, decodeOrFail)

import           Control.Lens (use)
import           PayChanServer.Config.Types (App, pubKey, openConfig, OpenConfig(..))
import           BlockchainAPI.Impl.ChainSo (chainSoAddressInfo, toEither)
import           Test.GenData (deriveMockFundingInfo, convertMockFundingInfo)
import           Data.Typeable
-- Check expiration date
import           ChanStore.Lib.Settlement (expiresEarlierThan)
import           PayChanServer.Config.Types (openConfig, openMinLengthHours)
import           Data.Time.Clock (getCurrentTime)



dummyKey :: HT.OutPoint
dummyKey = HT.OutPoint dummyTxId 0

dummyTxId :: HT.TxHash
dummyTxId = HT.TxHash "0000000000000000000000000000000000000000000000000000000000000000"

getAppRootURL :: MonadSnap m => BS.ByteString -> m String
getAppRootURL basePath = do
    serverName <- getsRequest rqServerName
    isSecure <- getsRequest rqIsSecure
    return $ channelRootURL isSecure serverName basePath

httpLocationSetActiveChannel :: MonadSnap m => BS.ByteString -> HT.OutPoint -> m ()
httpLocationSetActiveChannel basePath chanId = do
    chanRootURL <- getAppRootURL basePath
    modifyResponse $ setHeader "Location" (cs $ chanRootURL ++ activeChannelPath chanId)

fundingAddressFromParams :: RecvPubKey -> Handler App App HC.Address
fundingAddressFromParams serverPubKey =
    flip getFundingAddress' serverPubKey <$>
        getClientPubKey <*>
        getQueryArg "exp_time"

getClientPubKey :: Handler App App SendPubKey
getClientPubKey = MkSendPubKey <$> getQueryArg "client_pubkey"

getServerPubKey :: Handler App App RecvPubKey
getServerPubKey = use pubKey

---- Blockchain API ----
txInfoFromAddr :: MonadSnap m => HC.Address -> m TxInfo
txInfoFromAddr fundAddr = do
    maybeTxId <- liftIO $ txIDFromAddr (toString fundAddr)
    txId <- case maybeTxId of
        Nothing ->  userError $
            "Can't find any transactions paying to funding address " ++ cs (encode fundAddr)
        Just txid -> return txid
    eitherFundOut <- liftIO $ fundingOutInfoFromTxId (toString fundAddr) txId
    -- The API has just provided us with a txid above, if it can't find said txid
    -- something is wrong with the API, so we return an internal error in this case.
    either (const $ internalError ("Can't find funding transaction: " ++ show txId)) return eitherFundOut

-- | Return (hash,idx) if sufficiently confirmed
guardIsConfirmed :: MonadSnap m => Integer -> TxInfo -> m TxInfo
guardIsConfirmed minConf txInfo@(TxInfo _ txConfs (OutInfo _ _ _)) =
    if txConfs >= minConf then
        return txInfo
    else
        userError $ printf "Insufficient confirmation count for funding transaction: %d (need %d)" txConfs minConf
---- Blockchain API ----

channelIDFromPathArgs :: MonadSnap m => m HT.OutPoint
channelIDFromPathArgs =
    HT.OutPoint <$>
       getPathArg "funding_txid" <*>
       getPathArg "funding_vout"


--- Util ---
proceedIfExhausted :: MonadSnap m => (ChannelStatus,BitcoinAmount) -> m BitcoinAmount
proceedIfExhausted (ChannelOpen,_)          = finishWith =<< getResponse
proceedIfExhausted (ChannelClosed,valRecvd) = return valRecvd


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


checkExpirationTime :: BitcoinLockTime -> Handler App App  BitcoinLockTime
checkExpirationTime lockTime = do
    minDurationHours <- openMinLengthHours <$> use openConfig
    case lockTime of
        LockTimeBlockHeight _ ->
            userError "Block number as channel expiration date unsupported"
        LockTimeDate _ -> do
            now <- liftIO $ getCurrentTime
            let offsetSecs = fromIntegral $ minDurationHours * 3600
            if expiresEarlierThan (offsetSecs `addUTCTime` now) lockTime then
                    userError $ "Expiration date too early. Minimum channel duration: " ++
                        show minDurationHours ++ " hours"
                else
                    return lockTime

-- |Not used. A protocol feature that has been dropped, so far
--  (the client changing its change address for an open channel).
maybeUpdateChangeAddress :: MonadSnap m =>
    Maybe HC.Address
    -> ReceiverPaymentChannel
    -> m ReceiverPaymentChannel
maybeUpdateChangeAddress maybeAddr state =
    maybe (return state) updateAddressAndLog maybeAddr
        where updateAddressAndLog addr = do
                liftIO . putStrLn $ "Updating client change address to " ++ toString addr
                return $ setSenderChangeAddress state addr


--- Funding ---
blockchainGetFundingInfo :: Bool -> Handler App App FundingTxInfo
blockchainGetFundingInfo debug = fmap toFundingTxInfo $ do
    pubKeyServer <- getServerPubKey
    minConf <- openMinConf <$> use openConfig
    if (HCC.getNetworkName HCC.getNetwork == "testnet") && debug then
            test_GetDerivedFundingInfo pubKeyServer
        else
            fundingAddressFromParams pubKeyServer >>=
                blockchainAddressCheckEverything (fromIntegral minConf)

blockchainAddressCheckEverything :: MonadSnap m => Integer -> HC.Address -> m TxInfo
blockchainAddressCheckEverything minConf addr =
    (liftIO . chainSoAddressInfo . cs . HC.addrToBase58) addr >>=
        either internalError return . toEither >>=
        maybe
            (userError $ "No transactions paying to " ++ cs (HC.addrToBase58 addr) ++
                ". Maybe wait a little?")
            return >>=
        guardIsConfirmed minConf

-- | Deterministically derives a mock TxInfo from ChannelParameters,
-- which matches that of the test data generated by Test.GenData.
test_GetDerivedFundingInfo :: RecvPubKey -> Handler App App TxInfo
test_GetDerivedFundingInfo pubKeyServer = do
    cp <- flip CChannelParameters pubKeyServer <$>
            getClientPubKey <*>
            getQueryArg "exp_time"
    return $ convertMockFundingInfo . deriveMockFundingInfo $ cp
--- Funding ---



