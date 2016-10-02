{-# LANGUAGE OverloadedStrings #-}

module Util.Config.Parse where

import           Types
import           Types.Orphans ()
import           Util
import           Util.Config
import           Data.Configurator.Types
import qualified Servant.Common.BaseUrl as BaseUrl
import qualified Data.Tagged as Tag
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

parseBaseUrl :: Text -> Config -> IO BaseUrl.BaseUrl
parseBaseUrl keyPrefix cfg = BaseUrl.BaseUrl <$>
    configLookupOrFail cfg (keyPrefix <> ".protocol") <*>
    configLookupOrFail cfg (keyPrefix <> ".host") <*>
    configLookupOrFail cfg (keyPrefix <> ".port") <*>
    return ""   -- path prefix

parseTaggedBaseUrl :: Text -> Config -> IO (Tag.Tagged a BaseUrl.BaseUrl)
parseTaggedBaseUrl s = fmap Tag.Tagged . parseBaseUrl s

parseConnection :: Text -> Config -> IO (BaseUrl.BaseUrl, HTTP.Manager)
parseConnection p cfg = do
    baseUrl <- parseBaseUrl p cfg
    man <- HTTP.newManager (pickSettings baseUrl)
    return (baseUrl, man)
    where
        pickSettings (BaseUrl.BaseUrl BaseUrl.Http _ _ _)  = HTTP.defaultManagerSettings
        pickSettings (BaseUrl.BaseUrl BaseUrl.Https _ _ _) = HTTPS.tlsManagerSettings
