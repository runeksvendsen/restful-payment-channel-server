-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Common.ResourceURL where

import           Common.URLParam
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount, FullPayment, ChannelParameters(..),
                                                    SendPubKey, RecvPubKey, b64Encode,
                                                    IsPubKey(getPubKey))
import           Data.Bitcoin.PaymentChannel.Util
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Data.ByteString as BS
import           Data.String.Conversions (cs)
import           Text.Printf (printf)


channelRootURL :: Bool -> BS.ByteString -> BS.ByteString -> String
channelRootURL isSecure hostname basePath =
    printf "%s://%s%s"
        (if isSecure then "https" else "http" :: String)
        (cs hostname :: String)
        (cs basePath :: String)

channelOpenPath :: SendPubKey -> BitcoinLockTime -> String
channelOpenPath sendPK expTime = "/channels/new" ++
    printf "?client_pubkey=%s&exp_time=%s"
        (cs $ pathParamEncode (getPubKey sendPK) :: String)
        (cs $ pathParamEncode expTime :: String)

activeChannelPath :: HT.OutPoint -> String
activeChannelPath (HT.OutPoint txid vout)  = "/channels/" ++
    cs (pathParamEncode txid) ++ "/" ++
    cs (pathParamEncode vout)

-- https://localhost:8000/channels/f583e0b.../1
activeChannelURL :: Bool -> BS.ByteString -> BS.ByteString -> HT.OutPoint -> String
activeChannelURL isSecure host basePath chanId =
    channelRootURL isSecure (cs host) basePath ++ activeChannelPath chanId

-- /channels/new" -- ?client_pubkey&exp_time
channelOpenURL :: Bool -> String -> BS.ByteString -> SendPubKey -> BitcoinLockTime -> String
channelOpenURL isSecure host basePath sendPK expTime =
    channelRootURL isSecure (cs host) basePath ++ channelOpenPath sendPK expTime

mkOpenQueryParams :: HC.Address -> FullPayment -> String
mkOpenQueryParams chgAddr payment =
    printf "&change_address=%s&payment=%s"
        (cs $ pathParamEncode chgAddr :: String)
        (cs $ pathParamEncode payment :: String)

-- https://localhost:8000/channels/f583e0b.../1?payment=AAf8s...(&change_address=2Nuz3s...)
mkPaymentURL :: Bool -> String -> BS.ByteString -> HT.OutPoint -> FullPayment -> String
mkPaymentURL isSecure host basePath chanId payment  =
    activeChannelURL isSecure (cs host) basePath chanId ++ mkPaymentQueryParams payment Nothing

-- ?payment=AAf8s...(&change_address=2Nuz3s...)
mkPaymentQueryParams :: FullPayment -> Maybe HC.Address -> String
mkPaymentQueryParams payment maybeAddr =
    printf "?payment=%s"
        (cs $ pathParamEncode payment :: String) ++
    maybe "" (\addr -> "&change_address=" ++ (cs . pathParamEncode $ addr)) maybeAddr

