module AppPrelude.Util.URLParam where

import           Data.Configurator.Types
import qualified Servant.Common.BaseUrl as BaseUrl
import qualified Data.ByteString as BS
import qualified Data.Aeson as JSON
import           Data.Aeson (FromJSON, ToJSON)
import qualified AppPrelude.Util.Hex as Hex

import           PaymentChannel.Util (deserEither)

import qualified PaymentChannel.Types as Pay

import           Servant.API

import qualified Servant.API.ContentTypes as Content
import qualified Data.ByteString.Lazy as BL
import qualified Web.HttpApiData as Web


import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Util as HU
import qualified Data.Serialize as Bin
import           Data.Word (Word32)
import           Data.EitherR (fmapL)
import           Data.String.Conversions (cs)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)


-- URLParam
-- |Types that can be encoded to fit in a URL parameter
class URLParamEncode a where
    pathParamEncode :: a -> BS.ByteString

instance URLParamEncode HC.PubKeyC where
    pathParamEncode = HU.encodeHex . cs . Bin.encode

instance URLParamEncode Pay.RecvPubKey where
    pathParamEncode = pathParamEncode . Pay.getPubKey

instance URLParamEncode Pay.SendPubKey where
    pathParamEncode = pathParamEncode . Pay.getPubKey

instance URLParamEncode HT.TxHash where
    pathParamEncode = HT.txHashToHex

instance URLParamEncode Pay.LockTimeDate where
    pathParamEncode = cs . JSON.encode

instance URLParamEncode Integer where
    pathParamEncode = cs . show

instance URLParamEncode Word32 where
    pathParamEncode = cs . show

instance URLParamEncode HC.Address where
    pathParamEncode = HC.addrToBase58

instance URLParamEncode HT.OutPoint where
    pathParamEncode = HU.encodeHex . cs . Bin.encode

instance URLParamEncode Bool where
    pathParamEncode b = if b then "true" else "false"

instance URLParamEncode Pay.BtcAmount where
    pathParamEncode = cs . JSON.encode

instance URLParamEncode UTCTime where
    pathParamEncode t = pathParamEncode
        (round $ utcTimeToPOSIXSeconds t :: Integer)


-- |Types that can be decoded from an URL parameter
class URLParamEncode a => URLParamDecode a where
    pathParamDecode :: BS.ByteString -> Either String a

instance URLParamDecode HC.PubKeyC where
    pathParamDecode bs =
        decodeHex bs >>=
        fmapL ("failed to decode public key: " ++) . Bin.decode

instance URLParamDecode Pay.RecvPubKey where
    pathParamDecode bs = Pay.MkRecvPubKey <$> pathParamDecode bs

instance URLParamDecode Pay.SendPubKey where
    pathParamDecode bs = Pay.MkSendPubKey <$> pathParamDecode bs

instance URLParamDecode HT.TxHash where
    pathParamDecode bs =
        maybe (Left $ "failed to decode transaction hash: " ++ cs bs)
            Right (HT.hexToTxHash bs)

instance URLParamDecode HT.OutPoint where
    pathParamDecode bs =
        decodeHex bs >>=
        fmapL ("failed to decode outpoint: " ++) . Bin.decode

instance URLParamDecode Pay.LockTimeDate where
    pathParamDecode bs = maybe
        (Left "expiration time parse failure") Right (JSON.decode . cs $ bs)

instance URLParamDecode HC.Address where
    pathParamDecode bs = maybe
        (Left $ "Bitcoin address parse failure: " ++ show bs) Right (HC.base58ToAddr bs)

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

instance URLParamDecode Pay.BtcAmount where
    pathParamDecode bs = maybe
         (Left "bitcoin amount parse failure") Right (JSON.decode . cs $ bs)

instance URLParamDecode UTCTime where
    pathParamDecode bs = (posixSecondsToUTCTime . fromIntegral)
        <$> (decodeVout bs :: Either String Integer)

decodeVout :: (Num a, FromJSON a) => BS.ByteString -> Either String a
decodeVout bs = maybe
    (Left "failed to decode funding tx vout") Right (JSON.decode . cs $ bs)

decodeHex bs = maybe (Left "invalid hex string") Right (HU.decodeHex bs)
