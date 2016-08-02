module ConnManager.Types where


import           ChanStore.Lib.Types
import           Common.URLParam (pathParamEncode)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment)
import           Data.Bitcoin.PaymentChannel.Util (deserEither)
import qualified Data.Aeson.Types as JSON (Parser, parseMaybe, parseEither)
import qualified Data.Aeson as JSON (eitherDecode)

import qualified Network.Haskoin.Transaction as HT
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid ((<>))
import qualified Control.Exception as E
import           Control.Monad.Catch (SomeException(..))
import           Data.String.Conversions (cs)
import           Data.Typeable



-- Connection {
type Host = BS.ByteString
type Port = Word

data ConnManager = Conn Host Port Manager
-- Connection }

class IsParser m where
    runParser :: m a -> BL.ByteString -> Either String a

instance IsParser BinGet.Get where
    runParser parser bs =
        case BinGet.runGetOrFail parser bs of
            Left (_,_,e)    -> Left e
            Right (_,_,val) -> Right val

-- instance IsParser JSON.Parser where
--     runParser parser bs =
--         case JSON.eitherDecode parser bs of
--             Left (_,_,e)    -> Left e
--             Right (_,_,val) -> Right val


class HasReqParams a where
    rMethod      :: a -> BS.ByteString
    rPath        :: a -> BS.ByteString
    rQueryStr    :: a -> BS.ByteString
    rBody        :: a -> BL.ByteString
    -- |Used to ignore non-successful status codes
    rStatusErr   :: a -> Maybe (Status -> ResponseHeaders -> CookieJar -> Maybe E.SomeException)
    rQueryStr    = const BS.empty
    rStatusErr   = const Nothing
    rBody        = const BL.empty

