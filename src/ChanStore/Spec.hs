{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ChanStore.Spec where

import ChanStore.Lib.Types
import ConnManager.RequestRunner

import           Common.URLParam (pathParamEncode)
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment, BitcoinAmount)
import           Data.Bitcoin.PaymentChannel.Util (deserEither)

import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString as BS
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock (UTCTime)
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)


data Create             = Create                ReceiverPaymentChannel      deriving Typeable
data Get                = Get                   HT.OutPoint                 deriving Typeable
data Update             = Update                HT.OutPoint Payment         deriving Typeable
data ByExpSettleBegin   = ByExpSettleBegin      UTCTime                     deriving Typeable
data ByIdSettleBegin    = ByIdSettleBegin       Key                         deriving Typeable
data ByValueSettleBegin = ByValueSettleBegin    BitcoinAmount               deriving Typeable
data SettleFin          = SettleFin HT.OutPoint HT.TxHash                   deriving Typeable


-- basePath :: BS.ByteString
-- basePath = "/channels/"

-- TODO: Tie the knot: create routing table ('ChanStore.Main.site') from instances below

instance HasReqParams Create where
    rPath _                 = "/store/by_id/"
    rMethod                 = const "POST"
    rBody (Create rpc)      = Bin.encode rpc

instance HasReqParams Get where
    rPath (Get key)         = "/store/by_id/" <> pathParamEncode key
    rMethod                 = const "GET"
    rStatusErr              = const $ Just notFoundMeansNothing -- ignore 404

instance HasReqParams Update where
    rPath (Update key _)    = "/store/by_id/" <> pathParamEncode key
    rMethod                 = const "PUT"
    rBody (Update _ paym)   = Bin.encode paym

instance HasReqParams ByIdSettleBegin where
    rPath (ByIdSettleBegin key)     = "/settlement/begin/by_id/" <> pathParamEncode key
    rMethod                         = const "PUT"

instance HasReqParams ByExpSettleBegin where
    rPath (ByExpSettleBegin expDate)= "/settlement/begin/by_exp/" <> pathParamEncode expDate
    rMethod                         = const "PUT"

-- Begin settling the smallest number of payment channels possible to cover the
--  specified amount.
instance HasReqParams ByValueSettleBegin where
    rPath (ByValueSettleBegin minVal) = "/settlement/begin/by_value/" <> pathParamEncode minVal
    rMethod                             = const "PUT"

instance HasReqParams SettleFin where
    rPath (SettleFin key _)         = "/settlement/finish/by_id/" <> pathParamEncode key
    rMethod                         = const "POST"
    rBody (SettleFin _ settleTxId)  = Bin.encode settleTxId
