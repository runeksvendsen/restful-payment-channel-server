module AppPrelude.Types.Orphans
( module AppPrelude.Types.Orphans
, module Data.Aeson
)
where

import           Data.Configurator.Types
import qualified Servant.Common.BaseUrl as BaseUrl
import qualified Data.ByteString as BS
import qualified Data.Aeson as JSON
import           Data.Aeson (FromJSON, ToJSON)
import qualified AppPrelude.Util.Hex as Hex
import           AppPrelude.Util.URLParam
import qualified AppPrelude.Util as Util
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



instance Configured BaseUrl.Scheme where
    convert (String "http") = return BaseUrl.Http
    convert (String "HTTP") = return BaseUrl.Http
    convert (String "https") = return BaseUrl.Https
    convert (String "HTTPS") = return BaseUrl.Https
    convert _ = Nothing

instance JSON.ToJSON BS.ByteString where
    toJSON = JSON.String . Util.cs . Hex.hexEncode

instance JSON.FromJSON BS.ByteString where
    parseJSON = JSON.withText "Hex data" $ either fail return . Hex.hexDecode . Util.cs



instance Web.FromHttpApiData HT.OutPoint where
    parseUrlPiece = fmapL cs . pathParamDecode . cs
instance Web.ToHttpApiData HT.OutPoint where
    toUrlPiece = cs . pathParamEncode

instance Web.FromHttpApiData Pay.BtcAmount where
    parseUrlPiece = fmapL cs . pathParamDecode . cs
instance Web.ToHttpApiData Pay.BtcAmount where
    toUrlPiece = cs . pathParamEncode

instance Web.FromHttpApiData HT.TxHash where
    parseUrlPiece = fmapL cs . pathParamDecode . cs
instance Web.ToHttpApiData HT.TxHash where
    toUrlPiece = cs . pathParamEncode

instance Web.FromHttpApiData Pay.LockTimeDate where
    parseUrlPiece = fmapL cs . pathParamDecode . cs
instance Web.ToHttpApiData Pay.LockTimeDate where
    toUrlPiece = cs . pathParamEncode

instance Web.FromHttpApiData Pay.SendPubKey where
    parseUrlPiece = fmapL cs . Hex.hexDecode . cs
instance Web.ToHttpApiData Pay.SendPubKey where
    toUrlPiece = cs . Hex.hexEncode

instance Web.FromHttpApiData Pay.RecvPubKey where
    parseUrlPiece = fmapL cs . Hex.hexDecode . cs
instance Web.ToHttpApiData Pay.RecvPubKey where
    toUrlPiece = cs . Hex.hexEncode


instance Web.FromHttpApiData HC.Signature where
    parseUrlPiece = fmapL cs . Hex.hexDecode . cs
instance Web.ToHttpApiData HC.Signature where
    toUrlPiece = cs . Hex.hexEncode

instance Web.FromHttpApiData HC.PubKeyC where
    parseUrlPiece = fmapL cs . Hex.hexDecode . cs
instance Web.ToHttpApiData HC.PubKeyC where
    toUrlPiece = cs . Hex.hexEncode

instance Content.MimeUnrender Content.OctetStream [Pay.ServerPayChanX] where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream [Pay.ServerPayChanX] where
    mimeRender _ = BL.fromStrict . Bin.encode

instance Content.MimeUnrender Content.OctetStream Pay.ServerPayChanX where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream Pay.ServerPayChanX where
    mimeRender _ = BL.fromStrict . Bin.encode

instance Content.MimeUnrender Content.OctetStream Pay.SignedPayment where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream Pay.SignedPayment where
    mimeRender _ = BL.fromStrict . Bin.encode
