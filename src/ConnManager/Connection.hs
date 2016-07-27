module ConnManager.Connection where --TODO: move

import           ChanStoreServer.ChanStore.Types
import ChanStoreServer.ChanStore.ChanStore
-- impor  PayChanServer.Config.Types

import Control.Concurrent.STM (STM, atomically, throwSTM)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Network.Haskoin.Transaction as HT
import qualified Control.Exception.Base as Fail

import qualified Data.ByteString as BS
import           Text.Printf        (printf)

import           Network.HTTP.Client



newConnManager :: BS.ByteString -> Word -> IO ConnManager
newConnManager host port =
    Conn host port <$> newManager
        defaultManagerSettings {
            managerConnCount = 100
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

