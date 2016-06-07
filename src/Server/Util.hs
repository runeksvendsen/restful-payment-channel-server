{-# LANGUAGE OverloadedStrings #-}

module Server.Util where

import           Common.Common
import           Common.Types
-- import           Common.Types.Payment ((..))
import           BlockchainAPI (txIDFromAddr, fundingOutInfoFromTxId, txConfs, toFundingTxInfo,
                                TxInfo(..), OutInfo(..))
import           Server.Config (pubKeyServer)
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
-- import           Snap.Extras.JSON (reqBoundedJSON, writeJSON)

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
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Word (Word8,Word32)
import Data.Text as T
import Text.Printf (printf)
import Data.EitherR (fmapL)
import Data.String.Conversions (cs)
import Data.CaseInsensitive (CI, original)

--- TYPES ---
type Vout = Integer

data ChanOpenConfig = OpenConfig
    ChannelMap TxInfo HC.PubKey HC.Address BitcoinLockTime Payment

data ChanPayConfig = PayConfig
    ChannelMap HT.TxHash Vout (Maybe HC.Address) Payment

data ChanSettleConfig = SettleConfig {
    confPrivKey     :: HC.PrvKey,
    confRecvAddr    :: HC.Address,
    confTxFee       :: BitcoinAmount,

    serverChanMap   :: ChannelMap,

    chanHash        :: HT.TxHash,
    chanVout        :: Vout,
    chanPayment     :: Payment
    }

--- TYPES ---

fundingAddrFromParams sendPK = getFundingAddress' sendPK pubKeyServer

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


--- Read parameters with built-in error handling
failOnError :: MonadSnap m => Either String a -> m a
failOnError = either userError return

failOnNothingWith :: MonadSnap m => String -> Maybe a -> m a
failOnNothingWith s = maybe (userError s) return

getPathArg :: (MonadSnap m, PathParamDecode a) => BS.ByteString -> m a
getPathArg bs = failOnError . pathParamDecode =<<
    failOnNothingWith ("Missing " ++ C.unpack bs ++ " path parameter") =<<
        getParam bs

getQueryArg :: (MonadSnap m, PathParamDecode a) => BS.ByteString -> m a
getQueryArg bs = failOnError . pathParamDecode =<<
    failOnNothingWith ("Missing " ++ C.unpack bs ++ " query parameter") =<<
        getQueryParam bs

getOptionalQueryArg :: (MonadSnap m, PathParamDecode a) => BS.ByteString -> m (Maybe a)
getOptionalQueryArg bs = do
    maybeParam <- getQueryParam bs
    case maybeParam of
        Nothing -> return Nothing
        Just pBS -> fmap Just . failOnError . pathParamDecode $ pBS

--- Read parameters with built-in error handling

getHeaderOrFail :: MonadSnap m => CI BS.ByteString -> m BS.ByteString
getHeaderOrFail h = do
    r <- getRequest
    maybe (userError $ cs (original h) ++ " header not present") return (getHeader h r)

headerGetPayment :: MonadSnap m => m Payment
headerGetPayment = do
    pBS <- getHeaderOrFail "Payment-Payload"
    either
        (userError . ("failed to decode payment payload: " ++))
        return
        (eitherDecode $ cs pBS)

bodyJSONGetPayment :: MonadSnap m => m Payment
bodyJSONGetPayment =
    reqBoundedJSON 1024 >>=
    either (userError . ("Failed to parse Payment: " ++)) (return . paymentpayment_data)

---ERROR---
userError :: MonadSnap m => String -> m a
userError = errorWithDescription 400

internalError :: MonadSnap m => String -> m a
internalError = errorWithDescription 500

errorWithDescription :: MonadSnap m => Int -> String -> m a
errorWithDescription code errStr = do
    modifyResponse $ setResponseStatus 400 (C.pack errStr)
    finishWith =<< getResponse
---ERROR---


---JSON BODY RESPONSE---
decodeJSON :: (MonadSnap m, FromJSON a) => BS.ByteString -> m a
decodeJSON bs =
    either
        (userError . ("failed to decode JSON: " ++))
        return
        (eitherDecodeStrict bs)

reqBoundedJSON :: (MonadSnap m, FromJSON a) => Int64 -> m (Either String a)
reqBoundedJSON n = fmap eitherDecode (readRequestBody n)

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON json = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode json
---JSON BODY RESPONSE---


----External IP----
data ExternalIP = ExtIP String
instance FromJSON ExternalIP where
    parseJSON (Object v) = ExtIP <$> v .: "ip"
    parseJSON _          = mzero
instance Show ExternalIP where
    show (ExtIP s) = s

getExternalIP :: IO ExternalIP
getExternalIP =
    fmap (^. responseBody) $ asJSON =<< get "https://api.ipify.org?format=json"
----(External IP)----

