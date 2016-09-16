{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SigningService.Snap.Util where


import           Prelude hiding (userError)

import           Common.URLParam
import           Data.Bitcoin.PaymentChannel.Types (ChannelParameters(..), BitcoinLockTime(..),
                                                    SendPubKey(..),RecvPubKey(..))
import           Data.Bitcoin.PaymentChannel.Util (deserEither, getFundingAddress)
import           Snap
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Builder as Builder
import           Data.Aeson (ToJSON, encode)
import           Data.String.Conversions (cs)

import qualified Network.Haskoin.Crypto as HC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C

import           Data.Word (Word64)
import qualified Data.Serialize as Bin
import           Data.Typeable


--- Get URL parameters with built-in error handling
failOnError :: MonadSnap m => String -> Either String a -> m a
failOnError msg = either (userError . (msg ++)) return

failOnNothingWith :: MonadSnap m => String -> Maybe a -> m a
failOnNothingWith s = maybe (userError s) return

handleQueryDecodeFail :: MonadSnap m => BS.ByteString -> Either String a -> m a
handleQueryDecodeFail bs = failOnError ("failed to decode query arg \"" ++ cs bs ++ "\": ")

getPathArg :: (MonadSnap m, URLParamDecode a) => BS.ByteString -> m a
getPathArg bs = failOnError (cs bs ++ ": failed to decode path arg: ") . pathParamDecode =<<
    failOnNothingWith ("Missing " ++ C.unpack bs ++ " path parameter") =<<
        getParam bs

getQueryArg :: (MonadSnap m, URLParamDecode a) => BS.ByteString -> m a
getQueryArg bs = handleQueryDecodeFail bs . pathParamDecode =<<
    failOnNothingWith ("Missing " ++ C.unpack bs ++ " query parameter") =<<
        getQueryParam bs

getOptionalQueryArg :: (MonadSnap m, URLParamDecode a) => BS.ByteString -> m (Maybe a)
getOptionalQueryArg bs = do
    maybeParam <- getQueryParam bs
    case maybeParam of
        Nothing -> return Nothing
        Just pBS -> fmap Just . handleQueryDecodeFail bs . pathParamDecode $ pBS


--- HTTP error
userError :: MonadSnap m => String -> m a
userError = errorWithDescription 400

internalError :: MonadSnap m => String -> m a
internalError = errorWithDescription 500

errorWithDescription :: MonadSnap m => Int -> String -> m a
errorWithDescription code errStr = do
    modifyResponse $ setResponseStatus code (C.pack errStr)
    finishWith =<< getResponse


--- HTTP decode/write data
encodeJSON :: ToJSON a => a -> BS.ByteString
encodeJSON = BL.toStrict . encode

overwriteResponseBody :: MonadSnap m => BS.ByteString -> m ()
overwriteResponseBody bs = putResponse $
    setResponseBody
        (\out ->
            Streams.write (Just $ Builder.byteString bs) out >>
            return out)
        emptyResponse

writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON json = do
    modifyResponse $ setContentType "application/json"
    overwriteResponseBody $ encodeJSON json
    writeBS "\n"

writeBinary :: (MonadSnap m, Bin.Serialize a) => a -> m ()
writeBinary obj = do
    modifyResponse $ setContentType "application/octet-stream"
    overwriteResponseBody $ Bin.encode obj

decodeFromBody :: (MonadSnap m, Typeable a, Bin.Serialize a) => Word64 -> m a
decodeFromBody n =
    fmap (deserEither . cs) (readRequestBody n) >>=
     \eitherRes -> case eitherRes of
         Left e -> userError $ "Failed to decode from body: " ++ e
         Right val -> return val


-- TODO: Figure out how to bake this into all handlers.
--  Currently we apply it manually to each handler (snap-cors lib doesn't work for me)
applyCORS' :: MonadSnap m => m ()
applyCORS' = do
    modifyResponse $ setHeader "Access-Control-Allow-Origin"    "*"
    modifyResponse $ setHeader "Access-Control-Allow-Methods"   "GET,POST,PUT,DELETE"
    modifyResponse $ setHeader "Access-Control-Allow-Headers"   "Origin,Accept,Content-Type"
    modifyResponse $ setHeader "Access-Control-Expose-Headers"  "Location"


----Helpers
getFundingAddress' :: SendPubKey -> RecvPubKey -> BitcoinLockTime -> HC.Address
getFundingAddress' sendPK recvPK blt =
    getFundingAddress $ CChannelParameters sendPK recvPK blt 0

toString :: HC.Address -> String
toString = C.unpack . HC.addrToBase58

fromHexString :: String -> BS.ByteString
fromHexString hexStr =
    case (B16.decode . C.pack) hexStr of
        (bs,e) ->
            if BS.length e /= 0 then BS.empty else bs

