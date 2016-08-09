module ConnManager.Connection where --TODO: move

import           ConnManager.Types

-- import           ChanStore.Lib.Types
-- import           ChanStore.Lib.ChanMap



import qualified Data.ByteString as BS

import           Network.HTTP.Client



newConnManager :: BS.ByteString -> Word -> Int -> IO ConnManager
newConnManager host port numConns =
    Conn host port <$> newManager
        defaultManagerSettings {
            managerConnCount = numConns
        }

dummyRequest :: IO Request
dummyRequest = parseUrl "http://dummy.com/"

getBaseRequest :: ConnManager -> IO Request
getBaseRequest (Conn host port _) =
    dummyRequest >>= \req ->
        return $ req {
            secure = False,
            host = host,
            port = fromIntegral port
        }

