module PayChanServer.URI where

import qualified PayChanServer.API as API
-- import           PayChanServer.Types
import           Servant

api = Proxy :: Proxy API.RBPCP

beginOpenAPI = Proxy :: Proxy API.ChanOpen
mkChanURI = safeLink api beginOpenAPI

closeAPI = Proxy :: Proxy API.ChanClose
mkCloseURI = safeLink api closeAPI



