{-# LANGUAGE OverloadedStrings #-}

module Server.ChanStore.RequestRunner where

import           Server.ChanStore.Types
import           Server.ChanStore.Connection
import           Server.Util (decodeEither)
import           Common.Common (pathParamEncode)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment)
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

class ReqParams a where
    rPath        :: a -> BS.ByteString
    rMethod      :: a -> BS.ByteString
    rBody        :: a -> Maybe BS.ByteString
    rQueryStr    :: a -> Maybe BS.ByteString
    rStatusErr   :: a -> Maybe (Status -> ResponseHeaders -> CookieJar -> Maybe E.SomeException)

data Create = Create ReceiverPaymentChannel
data Get    = Get    HT.OutPoint
data Update = Update HT.OutPoint Payment
data Delete = Delete HT.OutPoint HT.TxHash

basePath :: BS.ByteString
basePath = "/channels/by_id/"

instance ReqParams Create where
    rPath              = const basePath
    rMethod            = const "POST"
    rBody (Create rpc) = Just . BL.toStrict $ Bin.encode rpc
    rQueryStr          = const Nothing
    rStatusErr         = const Nothing

instance ReqParams Get where
    rPath (Get key)    = basePath <> pathParamEncode key
    rMethod            = const "GET"
    rBody              = const Nothing
    rQueryStr          = const Nothing
    rStatusErr         = const $ Just notFoundMeansNothing -- ignore 404

instance ReqParams Update where
    rPath (Update key _)  = basePath <> pathParamEncode key
    rMethod               = const "PUT"
    rBody (Update _ paym) = Just . BL.toStrict $ Bin.encode paym
    rQueryStr             = const Nothing
    rStatusErr            = const Nothing

instance ReqParams Delete where
    rPath (Delete key _)     = basePath <> pathParamEncode key
    rMethod                  = const "DELETE"
    rBody                    = const Nothing
    rQueryStr (Delete _ tid) = Just $ "settlement_txid=" <> pathParamEncode tid
    rStatusErr               = const Nothing

requestFromParams :: ReqParams a => ChanMapConn -> a -> IO Request
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

runRequest :: (ReqParams a, Bin.Binary b) => ChanMapConn -> a -> IO b
runRequest conn@(Conn _ _ man) rp =
    requestFromParams conn rp >>= \req -> withResponse (installStatusHandler rp req) man
        ( \resp -> failOnLeft . decodeEither =<< responseBodyUnless404 resp )
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
