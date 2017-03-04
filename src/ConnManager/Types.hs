module ConnManager.Types where


import           AppPrelude.Man
import           AppPrelude.Util.URLParam (pathParamEncode)

import           PaymentChannel.Types (ServerPayChanX, SignedPayment)
import           PaymentChannel.Util (deserEither)
import qualified Data.Aeson.Types as JSON (Parser, parseMaybe, parseEither)

import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E

import           Servant.Client
import qualified Servant.Common.BaseUrl as BaseUrl
import           Data.String.Conversions (cs)


data ConnManager2 = Conn2 BaseUrl Manager

fromCM :: ConnManager -> ConnManager2
fromCM (Conn host port man) =
    Conn2 (BaseUrl.BaseUrl BaseUrl.Http (cs host) (fromIntegral port) "") man

-- Connection {
type Host = BS.ByteString
type Port = Word

data ConnManager = Conn Host Port Manager
-- Connection }


class HasReqParams a where
    rMethod      :: a -> BS.ByteString
    rPath        :: a -> BS.ByteString
    rQueryStr    :: a -> BS.ByteString
    rBody        :: a -> BS.ByteString
    -- |Used to ignore non-successful status codes
    rStatusErr   :: a -> Maybe (Status -> ResponseHeaders -> CookieJar -> Maybe E.SomeException)
    rQueryStr    = const BS.empty
    rStatusErr   = const Nothing
    rBody        = const BS.empty

