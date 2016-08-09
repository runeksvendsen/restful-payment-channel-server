module ConnManager.Servant where

import ConnManager.Types

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import           Servant
import qualified Control.Monad.Error.Class as Except



    -- runReq :: ConnManager2 -> Handler a
    -- runReq (Conn2 baseUrl man) = do
    --   res <- runExceptT (unspentOutputs addr man baseUrl)
    --   case res of
    --     Left err -> Except.throwError $ err500 { errBody = cs $ show err }
    --     Right a -> return a
