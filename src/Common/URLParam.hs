{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.URLParam where


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



-- |Types that can be encoded to fit in a URL parameter
class URLParamEncode a where
    pathParamEncode :: a -> BS.ByteString

instance URLParamEncode HC.PubKey where
    pathParamEncode = HU.encodeHex . cs . Bin.encode

instance URLParamEncode RecvPubKey where
    pathParamEncode recvPK = pathParamEncode $ getPubKey recvPK

instance URLParamEncode HT.TxHash where
    pathParamEncode = HT.txHashToHex

instance URLParamEncode BitcoinLockTime where
    pathParamEncode = cs . encode

instance URLParamEncode Integer where
    pathParamEncode = cs . show

instance URLParamEncode Word32 where
    pathParamEncode = cs . show

instance URLParamEncode HC.Address where
    pathParamEncode = HC.addrToBase58

instance URLParamEncode Payment where
    pathParamEncode = b64Encode

instance URLParamEncode HT.OutPoint where
    pathParamEncode = HU.encodeHex . cs . Bin.encode

instance URLParamEncode Bool where
    pathParamEncode b = if b then "true" else "false"

instance URLParamEncode BitcoinAmount where
    pathParamEncode = cs . encode

instance URLParamEncode UTCTime where
    pathParamEncode t = pathParamEncode
        (round $ utcTimeToPOSIXSeconds t :: Integer)


-- |Types that can be decoded from an URL parameter
class URLParamEncode a => URLParamDecode a where
    pathParamDecode :: BS.ByteString -> Either String a

instance URLParamDecode HC.PubKey where
    pathParamDecode bs =
        decodeHex bs >>=
        fmapL ("failed to decode public key: " ++) . HU.decodeToEither

instance URLParamDecode HT.TxHash where
    pathParamDecode bs =
        maybe (Left $ "failed to decode transaction hash: " ++ cs bs)
            Right (HT.hexToTxHash bs)

instance URLParamDecode HT.OutPoint where
    pathParamDecode bs =
        decodeHex bs >>=
        fmapL ("failed to decode outpoint: " ++) . HU.decodeToEither

instance URLParamDecode BitcoinLockTime where
    pathParamDecode bs = maybe
        (Left "expiration time parse failure") Right (decode . cs $ bs)

instance URLParamDecode HC.Address where
    pathParamDecode bs = maybe
        (Left $ "Bitcoin address parse failure: " ++ show bs) Right (HC.base58ToAddr bs)

instance URLParamDecode Payment where
    pathParamDecode bs =
        case fromJSON . String . cs $ bs of
            Error e -> Left $ "payment parse failure: " ++ e
            Success p -> Right p

instance URLParamDecode Integer where
    pathParamDecode = decodeVout

instance URLParamDecode Word32 where
    pathParamDecode = decodeVout

instance URLParamDecode Bool where
    pathParamDecode bs =
        case bs of
            "true" -> Right True
            "false" -> Right False
            _       -> Left "boolean must be either \"true\" or \"false\""

instance URLParamDecode BitcoinAmount where
    pathParamDecode bs = maybe
         (Left "bitcoin amount parse failure") Right (decode . cs $ bs)

instance URLParamDecode UTCTime where
    pathParamDecode bs = (posixSecondsToUTCTime . fromIntegral)
        <$> (decodeVout bs :: Either String Integer)

decodeVout :: (Num a, FromJSON a) => BS.ByteString -> Either String a
decodeVout bs = maybe
    (Left "failed to decode funding tx vout") Right (decode . cs $ bs)

decodeHex bs = maybe (Left "invalid hex string") Right (HU.decodeHex bs)