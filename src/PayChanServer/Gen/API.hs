{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, FlexibleInstances, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DeriveTraversable, FlexibleContexts, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=306 #-}
module PayChanServer.Gen.API (
  -- * Client and Server
  ServerConfig(..),
  RESTfulBitcoinPaymentChannelProtocolBackend,
  createRESTfulBitcoinPaymentChannelProtocolClient,
  runRESTfulBitcoinPaymentChannelProtocolServer,
  runRESTfulBitcoinPaymentChannelProtocolClient,
  runRESTfulBitcoinPaymentChannelProtocolClientWithManager,
  RESTfulBitcoinPaymentChannelProtocolClient,
  -- ** Servant
  RESTfulBitcoinPaymentChannelProtocolAPI,
  ) where

import PayChanServer.Gen.Types

import Data.Aeson (Value)
import Data.Coerce (coerce)
import Servant.API
import Servant (serve, ServantErr)
import Web.HttpApiData
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Text as T
import Data.Text (Text)
import Servant.Common.BaseUrl(BaseUrl(..))
import Servant.Client (ServantError, client, Scheme(Http))
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class
import Data.Function ((&))
import GHC.Exts (IsString(..))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Monoid ((<>))
import Servant.API.Verbs (Verb, StdMethod(..))
import Control.Monad.Except (ExceptT)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types.Method (methodOptions)

-- instance ReflectMethod 'OPTIONS where
--   reflectMethod _ = methodOptions




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either Text b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> key <> " in form data"
    Just value -> parseQueryParam value

-- | Servant type-level API, generated from the Swagger spec for RESTfulBitcoinPaymentChannelProtocol.
type RESTfulBitcoinPaymentChannelProtocolAPI
    =    "channels" :> Capture "client_pubkey" Text :> Capture "exp_time" Integer :> Capture "funding_txid" Text :> Capture "funding_vout" Text :> ReqBody '[JSON] PaymentData :> Verb 'POST 200 '[JSON] PaymentResult -- 'createPaymentChannel' route
    :<|> "channels" :> Capture "client_pubkey" Text :> Capture "exp_time" Integer :> Capture "funding_txid" Text :> Capture "funding_vout" Text :> QueryParam "change_address" Text :> ReqBody '[JSON] PaymentData :> Verb 'DELETE 200 '[JSON] PaymentResult -- 'deletePaymentChannel' route
    :<|> "funding" :> Capture "client_pubkey" Text :> Capture "exp_time" Integer :> "begin_open" :> Verb 'GET 200 '[JSON] ChannelInfo -- 'fundingBeginOpen' route
    :<|> "funding" :> Capture "client_pubkey" Text :> Capture "exp_time" Integer :> "info" :> Verb 'GET 200 '[JSON] FundingInfo -- 'getFundingInfo' route
    :<|> "channels" :> Capture "client_pubkey" Text :> Capture "exp_time" Integer :> Capture "funding_txid" Text :> Capture "funding_vout" Text :> ReqBody '[JSON] PaymentData :> Verb 'PUT 200 '[JSON] PaymentResult -- 'payPaymentChannel' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig {
    configHost :: String,  -- ^ Hostname to serve on, e.g. "127.0.0.1"
    configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList { fromQueryList :: [a] }
  deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat = CommaSeparated -- ^ CSV format for multiple parameters.
                      | SpaceSeparated -- ^ Also called "SSV"
                      | TabSeparated -- ^ Also called "TSV"
                      | PipeSeparated -- ^ `value1|value2|value2`
                      | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
    parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
    parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
    parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
    parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
    parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
    toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
    toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
    toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
    toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
    toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for RESTfulBitcoinPaymentChannelProtocol.
-- The backend can be used both for the client and the server. The client generated from the RESTfulBitcoinPaymentChannelProtocol Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createRESTfulBitcoinPaymentChannelProtocolClient@). Alternatively, provided
-- a backend, the API can be served using @runRESTfulBitcoinPaymentChannelProtocolServer@.
data RESTfulBitcoinPaymentChannelProtocolBackend m = RESTfulBitcoinPaymentChannelProtocolBackend {
    createPaymentChannel :: Text -> Integer -> Text -> Text -> PaymentData -> m PaymentResult{- ^ After publishing the funding Bitcoin transaction, and waiting for the specified number of confirmations, the client will POST a payment, paying the server's specified channel open price, to this URL. Subsequent channel payments will be PUT on this same URL.  -},
    deletePaymentChannel :: Text -> Integer -> Text -> Text -> Maybe Text -> PaymentData -> m PaymentResult{- ^ In order to enable the server to return change to the client, the client must create a new payment with the desired change address, and supply this pay as well as the change address in the appropriate query parameters. -},
    fundingBeginOpen :: Text -> Integer -> m ChannelInfo{- ^ Before opening a ...  -},
    getFundingInfo :: Text -> Integer -> m FundingInfo{- ^ Before opening a payment channel with the server, the client must first acquire the server public key, in order to calculate a funding address for the channel.  The client will first calculate the funding address, confirm that it matches the server's, then pay to the funding address, and wait until the funding transaction has the server-specified number of confirmations (\"funding_tx_min_conf\").  After this, the client will create a new payment of value equal to the server-specified channel open price (\"open_price\"), and POST this to the channel-open URL in order to open the channel.  -},
    payPaymentChannel :: Text -> Integer -> Text -> Text -> PaymentData -> m PaymentResult{- ^  -}
  }

newtype RESTfulBitcoinPaymentChannelProtocolClient a = RESTfulBitcoinPaymentChannelProtocolClient { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a }
    deriving Functor

instance Applicative RESTfulBitcoinPaymentChannelProtocolClient where
    pure x = RESTfulBitcoinPaymentChannelProtocolClient (\_ _ -> pure x)
    (RESTfulBitcoinPaymentChannelProtocolClient f) <*> (RESTfulBitcoinPaymentChannelProtocolClient x) = RESTfulBitcoinPaymentChannelProtocolClient (\manager url -> f manager url <*> x manager url)

instance Monad RESTfulBitcoinPaymentChannelProtocolClient where
    (RESTfulBitcoinPaymentChannelProtocolClient a) >>= f = RESTfulBitcoinPaymentChannelProtocolClient (\manager url -> do
        value <- a manager url
        runClient (f value) manager url)

instance MonadIO RESTfulBitcoinPaymentChannelProtocolClient where
    liftIO io = RESTfulBitcoinPaymentChannelProtocolClient (\_ _ -> liftIO io)

createRESTfulBitcoinPaymentChannelProtocolClient :: RESTfulBitcoinPaymentChannelProtocolBackend RESTfulBitcoinPaymentChannelProtocolClient
createRESTfulBitcoinPaymentChannelProtocolClient = RESTfulBitcoinPaymentChannelProtocolBackend{..}
  where
    ((coerce -> createPaymentChannel) :<|>
     (coerce -> deletePaymentChannel) :<|>
     (coerce -> fundingBeginOpen) :<|>
     (coerce -> getFundingInfo) :<|>
     (coerce -> payPaymentChannel)) = client (Proxy :: Proxy RESTfulBitcoinPaymentChannelProtocolAPI)

-- | Run requests in the RESTfulBitcoinPaymentChannelProtocolClient monad.
runRESTfulBitcoinPaymentChannelProtocolClient :: ServerConfig -> RESTfulBitcoinPaymentChannelProtocolClient a -> ExceptT ServantError IO a
runRESTfulBitcoinPaymentChannelProtocolClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runRESTfulBitcoinPaymentChannelProtocolClientWithManager manager clientConfig cl

-- | Run requests in the RESTfulBitcoinPaymentChannelProtocolClient monad using a custom manager.
runRESTfulBitcoinPaymentChannelProtocolClientWithManager :: Manager -> ServerConfig -> RESTfulBitcoinPaymentChannelProtocolClient a -> ExceptT ServantError IO a
runRESTfulBitcoinPaymentChannelProtocolClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the RESTfulBitcoinPaymentChannelProtocol server at the provided host and port.
runRESTfulBitcoinPaymentChannelProtocolServer :: MonadIO m => ServerConfig -> RESTfulBitcoinPaymentChannelProtocolBackend (ExceptT ServantErr IO)  -> m ()
runRESTfulBitcoinPaymentChannelProtocolServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy RESTfulBitcoinPaymentChannelProtocolAPI) (serverFromBackend backend)

  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend RESTfulBitcoinPaymentChannelProtocolBackend{..} =
      (coerce createPaymentChannel :<|>
       coerce deletePaymentChannel :<|>
       coerce fundingBeginOpen :<|>
       coerce getFundingInfo :<|>
       coerce payPaymentChannel)
