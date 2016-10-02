module PayChanServer.Callback.Interface
(
    module PayChanServer.Callback.Interface
  , API.PaymentInfo(..)
  , API.PaymentResponse(..)
)
 where

import qualified RBPCP.Callback as API
import           Types
import qualified Types.Config as Conf
import qualified Util.Config.Parse as ConfParse
import           Util.Config
import qualified Util.Servant.Run as Run

import qualified Servant.Common.BaseUrl as BaseUrl
import           Servant
import           Servant.Client
import qualified Network.HTTP.Client as HTTP


mkCallbackInterface :: (BaseUrl.BaseUrl , HTTP.Manager) -> Interface
mkCallbackInterface (baseUrl,man) =
    Interface
        (\pi -> failOnLeft =<< Run.runReq (valueReceived' pi man baseUrl) )
    where failOnLeft = either error return

data Interface = Interface {
    valueReceived    :: API.PaymentInfo -> IO API.PaymentResponse
}

api :: Proxy API.PaymentCallback
api = Proxy

valueReceived' = client api

instance Conf.FromConfig Interface where
    fromConf cfg = do
        enable <- configLookupOrFail cfg "valueCallback.enable"
        if not enable then
                -- Dummy function: return empty PaymentResponse
                return $ Interface $ const (return $ API.PaymentResponse "")
            else
                mkCallbackInterface <$> ConfParse.parseConnection "valueCallback" cfg

