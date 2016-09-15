{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.URLParam where

import           Common.Types
import           Data.Aeson   (Result(..), Value(String), FromJSON, fromJSON,  encode, decode)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Util as HU
import qualified Data.ByteString as BS
import qualified Data.Serialize as Bin
import           Data.Word (Word32)
import           Data.EitherR (fmapL)
import           Data.String.Conversions (cs)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)



-- |Types that can be encoded to fit in a URL parameter
class URLParamEncode a where
    pathParamEncode :: a -> BS.ByteString

instance URLParamEncode HC.PubKey where
    pathParamEncode = HU.encodeHex . cs . Bin.encode

instance URLParamEncode RecvPubKey where
    pathParamEncode = pathParamEncode . getPubKey

instance URLParamEncode SendPubKey where
    pathParamEncode = pathParamEncode . getPubKey

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
        fmapL ("failed to decode public key: " ++) . Bin.decode

instance URLParamDecode    RecvPubKey where
    pathParamDecode bs = MkRecvPubKey <$> pathParamDecode bs

instance URLParamDecode    SendPubKey where
    pathParamDecode bs = MkSendPubKey <$> pathParamDecode bs

instance URLParamDecode HT.TxHash where
    pathParamDecode bs =
        maybe (Left $ "failed to decode transaction hash: " ++ cs bs)
            Right (HT.hexToTxHash bs)

instance URLParamDecode HT.OutPoint where
    pathParamDecode bs =
        decodeHex bs >>=
        fmapL ("failed to decode outpoint: " ++) . Bin.decode

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