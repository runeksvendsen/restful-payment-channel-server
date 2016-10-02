module Types.Orphans where

import           Data.Configurator.Types
import qualified Servant.Common.BaseUrl as BaseUrl
import qualified Data.ByteString as BS
import qualified Data.Aeson as JSON
import qualified Util.Hex as Hex
import qualified Util as Util


instance Configured BaseUrl.Scheme where
    convert (String "http") = return BaseUrl.Http
    convert (String "https") = return BaseUrl.Https
    convert _ = Nothing

instance JSON.ToJSON BS.ByteString where
    toJSON = JSON.String . Util.cs . Hex.hexEncode

instance JSON.FromJSON BS.ByteString where
    parseJSON = JSON.withText "Hex data" $ either fail return . Hex.hexDecode . Util.cs
