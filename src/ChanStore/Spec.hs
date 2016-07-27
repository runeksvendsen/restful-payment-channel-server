{-# LANGUAGE OverloadedStrings #-}

module ChanStore.Spec where

import ChanStore.Lib.Types
import ConnManager.RequestRunner

import           Common.Common (pathParamEncode)
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment)

import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString as BS
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock (UTCTime)
import           Data.Monoid ((<>))


data Create = Create ReceiverPaymentChannel
data Get    = Get    HT.OutPoint
data Update = Update HT.OutPoint Payment
data ByExpSettleBegin = ByExpSettleBegin UTCTime
data ByIdSettleBegin  = ByIdSettleBegin Key
data SettleFin = SettleFin HT.OutPoint HT.TxHash


basePath :: BS.ByteString
basePath = "/channels/"

instance ReqParams Create where
    rPath              = const $ basePath <> "by_id/"
    rMethod            = const "POST"
    rBody (Create rpc) = Just . BL.toStrict $ Bin.encode rpc

instance ReqParams Get where
    rPath (Get key)    = basePath <> "by_id/" <> pathParamEncode key
    rMethod            = const "GET"
    rStatusErr         = const $ Just notFoundMeansNothing -- ignore 404

instance ReqParams Update where
    rPath (Update key _)  = basePath <> "by_id/" <> pathParamEncode key
    rMethod               = const "PUT"
    rBody (Update _ paym) = Just . BL.toStrict $ Bin.encode paym

instance ReqParams ByIdSettleBegin where
    rMethod                       = const "PUT"
    rPath (ByIdSettleBegin key) =         "/settlement/begin/by_id/" <> pathParamEncode key

instance ReqParams ByExpSettleBegin where
    rMethod                            = const "PUT"
    rPath (ByExpSettleBegin expDate) =    "/settlement/begin/by_exp/" <> pathParamEncode expDate

instance ReqParams SettleFin where
    rMethod                     = const "POST"
    rPath (SettleFin key _) = basePath <> "/settlement/finish/by_id/" <> pathParamEncode key
    rBody (SettleFin _ settleTxId) = Just . BL.toStrict $ Bin.encode settleTxId
