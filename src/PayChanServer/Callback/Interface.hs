module PayChanServer.Callback.Interface
(
    module PayChanServer.Callback.Interface
  , API.CallbackInfo(..)
  , API.CallbackResponse(..)
)
 where

import qualified RBPCP.Callback as API
import           AppPrelude.Types
import qualified AppPrelude.Types.Config        as Conf
import           AppPrelude.Util                  (envRead)
import qualified AppPrelude.Util.Config.Parse   as ConfParse
import           AppPrelude.Util.Config
import qualified AppPrelude.Util.Servant.Run    as Run

import qualified Servant.Common.BaseUrl         as BaseUrl
import qualified Network.HTTP.Client            as HTTP
import qualified Network.HTTP.Client.TLS        as HTTPS
import           Servant
import           Servant.Client
import qualified Data.Text              as T







mkCallbackInterface :: (BaseUrl.BaseUrl , HTTP.Manager) -> Interface
mkCallbackInterface (baseUrl,man) =
    Interface
        (\pi -> failOnLeft =<< Run.runReq (valueReceived' pi man baseUrl) )
    where failOnLeft = either error return

data Interface = Interface {
    valueReceived    :: API.CallbackInfo -> IO API.CallbackResponse
}

api :: Proxy API.PaymentCallback
api = Proxy

valueReceived' = client api

instance Conf.FromConfig Interface where
    fromConf _ = do
        connM <- parseEnvConn "CALLBACK"
        case connM of
            Just baseUrl -> do
                man <- HTTP.newManager (pickSettings baseUrl)
                return $ mkCallbackInterface (baseUrl, man)
            Nothing -> do
                putStrLn "INFO: Callback disabled."
                -- Dummy function: return empty PaymentResponse
                return $ Interface $ const (return $ API.CallbackResponse "" Nothing)
      where
        pickSettings (BaseUrl.BaseUrl BaseUrl.Http _ _ _)  = HTTP.defaultManagerSettings
        pickSettings (BaseUrl.BaseUrl BaseUrl.Https _ _ _) = HTTPS.tlsManagerSettings

parseEnvConn :: String -> IO (Maybe BaseUrl.BaseUrl)
parseEnvConn prefix = do
    hostM  <- envRead $ prefix ++ "_HOST"
    portM  <- envRead $ prefix ++ "_PORT"
    protoM <- envRead $ prefix ++ "_PROTO"
    case (hostM, portM, protoM) of
        (Just host, Just port, Just proto) -> do
            putStrLn $ "INFO: Callback endpoint: " ++
                T.unpack proto ++ "://" ++ host ++ ":" ++ show port
            return $ Just $ BaseUrl.BaseUrl (pickProto proto) host port ""
        _ -> return Nothing
  where
    pickProto txt
        | T.toUpper txt == "HTTP"  = BaseUrl.Http
        | T.toUpper txt == "HTTPS" = BaseUrl.Https
        | otherwise = error $
            "Bad protocol in CALLBACK_PROTO env. Expected \"http\" or \"https\". Found: " ++ show txt

