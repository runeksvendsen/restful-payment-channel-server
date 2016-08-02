{-# LANGUAGE OverloadedStrings #-}

module ConnManager.RequestRunner
(
    runRequest,
    runRequestJSON,
    notFoundMeansNothing,
    ConnManager,
    HasReqParams(..)
)

 where


import           ConnManager.Types

import           ChanStore.Lib.Types
import           ConnManager.Connection
import           Common.URLParam (pathParamEncode)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment)
import           Data.Bitcoin.PaymentChannel.Util (deserEither)

import qualified Data.Aeson as JSON   (FromJSON, eitherDecode)
import qualified Network.Haskoin.Transaction as HT
import qualified Data.Binary as Bin
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid ((<>))
import qualified Control.Exception as E
import           Control.Monad.Catch (SomeException(..))
import           Data.String.Conversions (cs)
import           Data.Typeable



requestFromParams :: (HasReqParams a) => ConnManager -> a -> IO Request
requestFromParams conn rp =
        getBaseRequest conn >>= \req -> return req {
            path = rPath rp,
            method = rMethod rp,
            requestBody = RequestBodyBS . BL.toStrict $ rBody rp,
            queryString = rQueryStr rp
    }


runRequest :: (HasReqParams a, Typeable b, Bin.Binary b) => ConnManager -> a -> IO b
runRequest conn@(Conn _ _ man) rp =
    let
        failOnLeft = either (fail . ("failed to parse binary response: " ++)) return
        responseBodyUnless404 res = if responseStatus res == notFound404 then return BL.empty
                                else BL.fromStrict . BS.concat <$> brConsume (responseBody res)
        parseResponseOrFail resp = failOnLeft . deserEither . cs =<< responseBodyUnless404 resp
    in
        requestFromParams conn rp >>=
            \req -> withResponse (installStatusHandler rp req) man parseResponseOrFail


runRequestJSON :: (HasReqParams a, JSON.FromJSON b) => ConnManager -> a -> IO b
runRequestJSON conn@(Conn _ _ connManager) rp =
    let
        failOnLeft = either (fail . ("failed to parse JSON response: " ++)) return
        withRequestResponse reqType man f req = withResponse (installStatusHandler reqType req) man f
        getResponseBody res = BL.fromStrict . BS.concat <$> brConsume (responseBody res)
        parseResponseOrFail resp = failOnLeft . JSON.eitherDecode . cs =<< getResponseBody resp
    in
        requestFromParams conn rp >>= withRequestResponse rp connManager parseResponseOrFail


installStatusHandler rp req =
    case rStatusErr rp of
        Just handler -> req { checkStatus = handler }
        Nothing -> req

notFoundMeansNothing :: Status -> ResponseHeaders -> CookieJar -> Maybe E.SomeException
notFoundMeansNothing s h c
    | s == status404 = Nothing
    | otherwise =
        if statusIsClientError s || statusIsServerError s then
            Just . SomeException $ StatusCodeException s h c
        else
            Nothing






--
-- runRequest conn@(Conn _ _ man) (Get    key) =
--     getBaseRequest conn >>= \req -> withResponse req {
--         path = "/channels/" <> pathParamEncode key,
--         method = "GET"
--      } man (\res -> failOnLeft =<< decodeEither =<< brConsume . responseBody $ res)
--         where failOnLeft = either (fail "failed to parse ReceiverPaymentChannel") return
--
-- runRequest conn@(Conn _ _ man) (Update key payment) =
--     getBaseRequest conn >>= \req -> withResponse req {
--         path = "/channels/" <> pathParamEncode key,
--         method = "PUT",
--         requestBody =  RequestBodyLBS $ Bin.encode payment
--      } man (const $ return ())
--
-- runRequest conn@(Conn _ _ man) (Delete key settleTxId) =
--     getBaseRequest conn >>= \req -> withResponse req {
--         path = "/channels/" <> pathParamEncode key,
--         method = "DELETE",
--         queryString = "settlement_txid=" <> pathParamEncode settleTxId
--      } man (const $ return ())
--
