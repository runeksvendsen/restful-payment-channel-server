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


-- basePath :: BS.ByteString
-- basePath = "/channels/"

-- TODO: Tie the knot: create routing table ('ChanStore.Main.site') from instances below

instance ReqParams Create where
    rPath _                 = "/store/by_id/"
    rMethod                 = const "POST"
    rBody (Create rpc)      = Just . BL.toStrict $ Bin.encode rpc

instance ReqParams Get where
    rPath (Get key)         = "/store/by_id/" <> pathParamEncode key
    rMethod                 = const "GET"
    rStatusErr              = const $ Just notFoundMeansNothing -- ignore 404

instance ReqParams Update where
    rPath (Update key _)    = "/store/by_id/" <> pathParamEncode key
    rMethod                 = const "PUT"
    rBody (Update _ paym)   = Just . BL.toStrict $ Bin.encode paym

instance ReqParams ByIdSettleBegin where
    rPath (ByIdSettleBegin key)     = "/settlement/begin/by_id/" <> pathParamEncode key
    rMethod                         = const "PUT"

instance ReqParams ByExpSettleBegin where
    rPath (ByExpSettleBegin expDate)= "/settlement/begin/by_exp/" <> pathParamEncode expDate
    rMethod                         = const "PUT"

instance ReqParams SettleFin where
    rPath (SettleFin key _)         = "/settlement/finish/by_id/" <> pathParamEncode key
    rMethod                         = const "POST"
    rBody (SettleFin _ settleTxId)  = Just . BL.toStrict $ Bin.encode settleTxId
