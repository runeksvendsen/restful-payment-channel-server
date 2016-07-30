-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Common.ResourceURL where


import           Common.URLParam

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount, Payment, ChannelParameters(..),
                                                    SendPubKey, RecvPubKey, b64Encode,
                                                    IsPubKey(getPubKey))

import           Data.Aeson
    (Result(..), Value(Number, Object, String), FromJSON, ToJSON, parseJSON, toJSON,
    fromJSON, withScientific, eitherDecodeStrict, encode, decode, (.=), (.:), object)

import           Data.Bitcoin.PaymentChannel.Util
import           Data.Bitcoin.PaymentChannel.Types (RecvPubKey(..), IsPubKey(getPubKey))

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Util as HU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
import           Data.Word (Word32)
import           Data.EitherR (fmapL)
import           Data.String.Conversions (cs)
import           Text.Printf (printf)
import qualified Data.Binary as Bin
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)







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

--- Test URLS
-- /channels/new" -- ?client_pubkey&exp_time
channelOpenURL :: Bool -> String -> BS.ByteString -> SendPubKey -> BitcoinLockTime -> String
channelOpenURL isSecure host basePath sendPK expTime =
    channelRootURL isSecure (cs host) basePath ++ channelOpenPath sendPK expTime

mkOpenQueryParams :: HC.Address -> Payment -> String
mkOpenQueryParams chgAddr payment =
    printf "&change_address=%s&payment=%s"
        (cs $ pathParamEncode chgAddr :: String)
        (cs $ pathParamEncode payment :: String)

mkPaymentURL :: Bool -> String -> BS.ByteString -> HT.OutPoint -> Payment -> String
mkPaymentURL isSecure host basePath chanId payment  =
    activeChannelURL isSecure (cs host) basePath chanId ++ mkPaymentQueryParams payment Nothing

-- ?payment=AAf8s...(&change_address=2Nuz3s...)
mkPaymentQueryParams :: Payment -> Maybe HC.Address -> String
mkPaymentQueryParams payment maybeAddr =
    printf "?payment=%s"
        (cs $ pathParamEncode payment :: String) ++
    maybe "" (\addr -> "&change_address=" ++ (cs . pathParamEncode $ addr)) maybeAddr

-- https://localhost/channels/f583e0b.../1
activeChannelURL :: Bool -> BS.ByteString -> BS.ByteString -> HT.OutPoint -> String
activeChannelURL isSecure host basePath chanId =
    channelRootURL isSecure (cs host) basePath ++ activeChannelPath chanId
--- Test URLs

----URLs-----
