{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Common where

import           Common.Types

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (Payment, ChannelParameters(..))


import           Control.Monad (mzero)
import           Control.Monad.IO.Class (liftIO)

import           Control.Lens ((^.))
import           Network.Wreq (get, asJSON, responseBody, Response)

import qualified Crypto.Secp256k1 as Secp
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types (Parser, parseMaybe, parseEither)
import           Data.Aeson
    (Result(..), Value(Number, Object, String), FromJSON, ToJSON, parseJSON, toJSON,
    fromJSON, withScientific, eitherDecodeStrict, encode, decode, (.=), (.:), object)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific, toBoundedInteger, scientific, coefficient)
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock (getCurrentTime)
import           Data.Bitcoin.PaymentChannel.Util
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Util as HU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
import Data.Word (Word8, Word32)
import Data.List (filter)
import Data.Maybe (listToMaybe)
import Data.EitherR (fmapL)
import Data.String.Conversions (cs)
import Text.Printf (printf)
import qualified Data.Binary as Bin


----
-- |Types that can be encoded to fit in a URI path
class PathParamEncode a where
    pathParamEncode :: a -> BS.ByteString

instance PathParamEncode HC.PubKey where
    pathParamEncode = HU.encodeHex . cs . Bin.encode

instance PathParamEncode HT.TxHash where
    pathParamEncode = HU.encodeHex . cs . Bin.encode

instance PathParamEncode BitcoinLockTime where
    pathParamEncode = cs . encode

instance PathParamEncode Integer where
    pathParamEncode = cs . show

----

----
-- |Types that can be decoded from a URI path component
class PathParamDecode a where
    pathParamDecode :: BS.ByteString -> Either String a

decodeHex bs = maybe (Left "invalid hex string") Right (HU.decodeHex bs)

instance PathParamDecode HC.PubKey where
    pathParamDecode bs =
        decodeHex bs >>=
        fmapL ("failed to decode public key: " ++) . HU.decodeToEither

instance PathParamDecode HT.TxHash where
    pathParamDecode bs =
        decodeHex bs >>=
        fmapL ("failed to decode transaction hash: " ++) . HU.decodeToEither

instance PathParamDecode BitcoinLockTime where
    pathParamDecode bs = maybe
        (Left "failed to decode expiration time") Right (decode . cs $ bs)

instance PathParamDecode HC.Address where
    pathParamDecode bs = maybe
        (Left $ "failed to decode Bitcoin address: " ++ show bs) Right (HC.base58ToAddr bs)

instance PathParamDecode Payment where
    pathParamDecode bs =
        decodeHex bs >>=
        fmapL ("failed to decode payment: " ++) . HU.decodeToEither

instance PathParamDecode Integer where
    pathParamDecode bs = maybe
        (Left "failed to decode funding tx vout") Right (decode . cs $ bs)
----


----URLs----
channelRootURL :: String -> String
channelRootURL host = "http://" ++ host -- ++ "/v1"

fundingInfoURL :: String -> HC.PubKey -> BitcoinLockTime ->  String
fundingInfoURL host sendPK expTime =
    channelRootURL host ++ "/fundingInfo" ++
    printf "?client_pubkey=%s&exp_time=%d"
        (cs $ pathParamEncode sendPK :: String)
        (toWord32 expTime)

-- /channels/new" -- ?client_pubkey&exp_time&change_address
channelOpenURL :: String -> HC.PubKey -> BitcoinLockTime -> String
channelOpenURL host sendPK expTime =
    channelRootURL host ++ "/channels/new" ++
        printf "?client_pubkey=%s&exp_time=%d"
            (cs $ pathParamEncode sendPK :: String)
            (toWord32 expTime)

-- http://localhost/channels/f583e0b.../1
activeChannelURL :: String -> HT.TxHash -> Integer -> String
activeChannelURL host txid vout =
    channelRootURL host ++ "/channels/" ++
        cs (pathParamEncode txid) ++ "/" ++
        cs (pathParamEncode vout)
----URLs-----


getFundingAddress' :: HC.PubKey -> HC.PubKey -> BitcoinLockTime -> HC.Address
getFundingAddress' sendPK recvPK blt =
    getFundingAddress $ CChannelParameters sendPK recvPK blt

addressFromMessages :: OpenRequest -> OpenResponse -> HC.Address
addressFromMessages (OpenRequest _ sendPK) (OpenResponse recvPK _ blt _) =
    getFundingAddress' sendPK recvPK blt

toString :: HC.Address -> String
toString = C.unpack . HC.addrToBase58











data OpenRequest = OpenRequest {
    reqDuration ::  Word32,
    reqPubKey   ::  HC.PubKey
} deriving Show

data OpenResponse = OpenResponse {
    resPubKey   ::  HC.PubKey,
    resMinConf  ::  Integer,
    resLockTime ::  BitcoinLockTime,
    resFundAddr ::  HC.Address
} deriving Show

data OpenFinish = OpenFinish {
    ofinFundAddr ::  HC.Address,
    ofinTxId     ::  HT.TxHash,
    ofinPayment  ::  Payment
} deriving Show

-----
instance ToJSON OpenFinish where
    toJSON (OpenFinish addr tid pmnt) = object
        [ "fundingAddress" .= addr, "fundingTxId" .= tid
        , "initialPayment" .= pmnt ]

instance FromJSON OpenFinish where
    parseJSON (Object v) = OpenFinish <$>
        v .: "fundingAddress" <*> v .: "fundingTxId" <*>
        v .: "initialPayment"
    parseJSON _          = mzero
-----

-----
instance ToJSON OpenRequest where
    toJSON (OpenRequest lt pk) = object [ "lockTime" .= lt, "senderPubKey" .= pk ]

instance FromJSON OpenRequest where
    parseJSON (Object v) =
        OpenRequest <$> v .: "lockTime" <*> v .: "senderPubKey"
    parseJSON _          = mzero
-----

-----
instance ToJSON OpenResponse where
    toJSON (OpenResponse pk conf lt addr) =
        object [ "receiverPubKey" .= pk, "minConfFundingTx" .= conf,
            "channelExpirationDate" .= lt, "channelFundingAddress" .= addr ]

instance FromJSON OpenResponse where
    parseJSON (Object v) =
        OpenResponse <$> v .: "receiverPubKey" <*> v .: "minConfFundingTx"
            <*> v .: "channelExpirationDate" <*> v .: "channelFundingAddress"
    parseJSON _          = mzero
-----



-----

newtype Satoshi = Satoshi Integer deriving (Eq, Num, Ord, Enum, Real, Integral)
instance Bounded Satoshi where
    minBound = Satoshi 0
    maxBound = Satoshi $ round $ 21e6 * 1e8 -- Will work fine for a Word32, too.

instance ToJSON BitcoinLockTime where
    toJSON blt = Number $ scientific
        (fromIntegral $ toWord32 blt) 0

instance FromJSON BitcoinLockTime where
    parseJSON = withScientific "BitcoinLockTime" $
        fmap (parseBitcoinLocktime . fromIntegral) . parseJSONInt

parseJSONInt :: Scientific -> Parser Integer
parseJSONInt s =
    case toBoundedInteger s of
        Just (Satoshi i) -> return i
        Nothing -> fail $ "failed to decode JSON number to integer. data: " ++ show s

-----

toJSONNum :: Integral a => a -> Value
toJSONNum i = Number $ scientific (toInteger i) 0

fromHexString :: String -> BS.ByteString
fromHexString hexStr =
    case (B16.decode . C.pack) hexStr of
        (bs,e) ->
            if BS.length e /= 0 then BS.empty else bs

hashToStr :: HT.TxHash -> String
hashToStr = C.unpack . HT.txHashToHex

parseBTCAmount = withScientific "bitcoin amount" $
    parseJSONInt . (* 1e8)

prettyShow :: Value -> String
prettyShow = C.unpack . toStrict . encodePretty
