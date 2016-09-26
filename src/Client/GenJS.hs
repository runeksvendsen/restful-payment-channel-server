module Client.GenJS where

import Servant
import Servant.JS
import qualified PayChanServer.API as API

api :: Proxy API.RBPCP
api = Proxy

main :: IO ()
main =
    writeJSForAPI api vanillaJS ("vanilla-api.js")