module PayChanServer.URI where

import qualified PayChanServer.API as API
import           Servant

api = Proxy :: Proxy API.RBPCP

openAPI = Proxy :: Proxy API.ChanOpen
mkChanURI = safeLink api openAPI

closeAPI = Proxy :: Proxy API.ChanClose
mkCloseURI = safeLink api closeAPI



