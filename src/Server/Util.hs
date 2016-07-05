{-# LANGUAGE OverloadedStrings #-}

module Server.Util where

import           Common.Common
import           Common.Types

import           BlockchainAPI.Impl.BlockrIo (txIDFromAddr, fundingOutInfoFromTxId)
import           BlockchainAPI.Types (txConfs, toFundingTxInfo,
                                TxInfo(..), OutInfo(..))
import           Server.ChanStore (ChannelMap, ChanState(..))
import           DiskStore (addItem, getItem)

import           Prelude hiding (userError)
import           Control.Monad (mzero, forM, unless)
import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Monad.State (gets)
import           Snap.Core
import           Snap.Core (getHeader)
-- import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Iteratee (Enumerator, enumBuilder)
import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ChannelParameters(..), PayChanError(..)
                                                    ,getChannelState, BitcoinAmount, Payment)
import           Data.Bitcoin.PaymentChannel.Util (getFundingAddress, BitcoinLockTime(..), fromDate)

import qualified Network.Haskoin.Constants as HCC
import Network.Wreq (get, post, asJSON, responseBody)
import Control.Lens ((^.))
import           Control.Monad.IO.Class
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT
import qualified Crypto.Secp256k1 as Secp
import           Data.Aeson
    (Result(..), Value(Number, Object, String), FromJSON, ToJSON, parseJSON, toJSON,
    fromJSON, withScientific, eitherDecode, eitherDecodeStrict, decode, encode, (.=), (.:), object)

import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Word (Word8,Word32)
import Data.Text as T
import Text.Printf (printf)
import Data.EitherR (fmapL)
import Data.String.Conversions (cs)
import Data.CaseInsensitive (CI, original)
import Data.Aeson.Encode.Pretty (encodePretty)



getAppRootURL :: MonadSnap m => BS.ByteString -> m String
getAppRootURL basePath = do
    serverName <- getsRequest rqServerName
    isSecure <- getsRequest rqIsSecure
    return $ channelRootURL isSecure serverName basePath

httpLocationSetActiveChannel :: MonadSnap m => BS.ByteString -> HT.TxHash -> Integer -> m ()
httpLocationSetActiveChannel basePath hash idx = do
    chanRootURL <- getAppRootURL basePath
    modifyResponse $ setHeader "Location" (cs $ chanRootURL ++ activeChannelPath hash idx)


fundingAddressFromParams :: MonadSnap m => HC.PubKey -> m HC.Address
fundingAddressFromParams pubKey =
    flip getFundingAddress' pubKey <$>
        getQueryArg "client_pubkey" <*>
        getQueryArg "exp_time"


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
guardIsConfirmed minConf txInfo@(TxInfo txId txConfs (OutInfo _ _ idx)) =
    if txConfs >= minConf then
        return txInfo
    else
        userError $ printf "Insufficient confirmation count for funding transaction: %d (need %d)" txConfs minConf
---- Blockchain API ----


--- Get parameters with built-in error handling
failOnError :: MonadSnap m => String -> Either String a -> m a
failOnError msg = either (userError . (msg ++)) return

failOnNothingWith :: MonadSnap m => String -> Maybe a -> m a
failOnNothingWith s = maybe (userError s) return

handleQueryDecodeFail :: MonadSnap m => BS.ByteString -> Either String a -> m a
handleQueryDecodeFail bs = failOnError ("failed to decode query arg \"" ++ cs bs ++ "\": ")

getPathArg :: (MonadSnap m, PathParamDecode a) => BS.ByteString -> m a
getPathArg bs = failOnError (cs bs ++ ": failed to decode path arg: ") . pathParamDecode =<<
    failOnNothingWith ("Missing " ++ C.unpack bs ++ " path parameter") =<<
        getParam bs

getQueryArg :: (MonadSnap m, PathParamDecode a) => BS.ByteString -> m a
getQueryArg bs = handleQueryDecodeFail bs . pathParamDecode =<<
    failOnNothingWith ("Missing " ++ C.unpack bs ++ " query parameter") =<<
        getQueryParam bs

getOptionalQueryArg :: (MonadSnap m, PathParamDecode a) => BS.ByteString -> m (Maybe a)
getOptionalQueryArg bs = do
    maybeParam <- getQueryParam bs
    case maybeParam of
        Nothing -> return Nothing
        Just pBS -> fmap Just . handleQueryDecodeFail bs . pathParamDecode $ pBS
--- Get parameters with built-in error handling

---ERROR---
userError :: MonadSnap m => String -> m a
userError = errorWithDescription 400

internalError :: MonadSnap m => String -> m a
internalError = errorWithDescription 500

errorWithDescription :: MonadSnap m => Int -> String -> m a
errorWithDescription code errStr = do
    modifyResponse $ setResponseStatus code (C.pack errStr)
    finishWith =<< getResponse
---ERROR---


encodeJSON :: ToJSON a => a -> BL.ByteString
encodeJSON json = encodePretty json

overwriteResponseBody :: MonadSnap m => BL.ByteString -> m ()
overwriteResponseBody bs =
    getResponse >>=
    putResponse . setResponseBody (enumBuilder $ fromLazyByteString bs)

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON json = do
    modifyResponse $ setContentType "application/json"
    overwriteResponseBody $ encodeJSON json
    writeBS "\n"




-- reqBoundedJSON :: (MonadSnap m, FromJSON a) => Int64 -> m (Either String a)
-- reqBoundedJSON n = fmap eitherDecode (readRequestBody n)
--
-- getHeaderOrFail :: MonadSnap m => CI BS.ByteString -> m BS.ByteString
-- getHeaderOrFail h = do
--     r <- getRequest
--     maybe (userError $ cs (original h) ++ " header not present") return (getHeader h r)
--
-- headerGetPayment :: MonadSnap m => m Payment
-- headerGetPayment = do
--     pBS <- getHeaderOrFail "Payment-Payload"
--     case fromJSON . String . cs $ pBS of
--         Error e -> userError $ "failed to decode payment payload: " ++ e
--         Success p -> return p
--
-- bodyJSONGetPayment :: MonadSnap m => m Payment
-- bodyJSONGetPayment =
--     reqBoundedJSON 1024 >>=
--     either (userError . ("Failed to parse Payment: " ++)) (return . paymentpayment_data)
