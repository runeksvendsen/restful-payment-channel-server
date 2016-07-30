{-# LANGUAGE OverloadedStrings #-}

module ConnManager.RequestRunner where

import           ChanStore.Lib.Types
import           ConnManager.Connection
import           Common.URLParam (pathParamEncode)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment)
import           Data.Bitcoin.PaymentChannel.Util (deserEither)

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

class ReqParams a where
    rPath        :: a -> BS.ByteString
    rMethod      :: a -> BS.ByteString
    rBody        :: a -> Maybe BS.ByteString
    rQueryStr    :: a -> Maybe BS.ByteString
    rStatusErr   :: a -> Maybe (Status -> ResponseHeaders -> CookieJar -> Maybe E.SomeException)
    rBody        = const Nothing
    rQueryStr    = const Nothing
    rStatusErr   = const Nothing


requestFromParams :: ReqParams a => ConnManager -> a -> IO Request
requestFromParams conn rp =
    let maybeJustOrDefault _   (Just a) = a
        maybeJustOrDefault def Nothing  = def
    in
        getBaseRequest conn >>= \req -> return req {
            path = rPath rp,
            method = rMethod rp,
            requestBody = RequestBodyBS $ maybeJustOrDefault BS.empty (rBody rp),
            queryString = maybeJustOrDefault BS.empty (rQueryStr rp)
    }

-- runRequest :: (ReqParams a, Bin.Binary b) => BS.ByteString -> Word -> ConnManager -> a -> IO b
-- runRequest host port conn@(Conn _ _ man) rp =
runRequest :: (ReqParams a, Typeable b, Bin.Binary b) => ConnManager -> a -> IO b
runRequest conn@(Conn _ _ man) rp =
    requestFromParams conn rp >>= \req -> withResponse (installStatusHandler rp req) man
        ( \resp -> failOnLeft . deserEither . cs =<< responseBodyUnless404 resp )
            where failOnLeft = either (fail . ("failed to parse response: " ++)) return
                  responseBodyUnless404 res =
                       if responseStatus res == notFound404 then return BL.empty
                            else BL.fromStrict . BS.concat <$> brConsume (responseBody res)


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
