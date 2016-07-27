{-# LANGUAGE OverloadedStrings #-}

module SigningService.Spec where

import           ConnManager.RequestRunner (ReqParams(..))
import           ChanStoreServer.ChanStore.Types
import           ConnManager.Connection
-- import           Data.Bitcoin.PaymentChannel.Util (deserEither)
import           Common.Common (pathParamEncode)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount)
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


data GetPubKey  = GetPubKey
data SettleChan = SettleChan ReceiverPaymentChannel BitcoinAmount


basePath :: BS.ByteString
basePath = "/"

instance ReqParams GetPubKey where
    rPath       = const $ basePath <> "get_pubkey"
    rMethod     = const "GET"

instance ReqParams SettleChan where
    rPath       = const $ basePath <> "settle_channel"
    rMethod     = const "POST"
    rQueryStr (SettleChan _ txFee) = Just $ "tx_fee=" <> pathParamEncode txFee
    rBody (SettleChan rpc _) =
        Just . BL.toStrict $ Bin.encode rpc
