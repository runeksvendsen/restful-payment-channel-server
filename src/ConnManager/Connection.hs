module ConnManager.Connection where --TODO: move

import           ChanStore.Lib.Types
import ChanStore.Lib.ChanMap
-- impor  PayChanServer.Config.Types

import Control.Concurrent.STM (STM, atomically, throwSTM)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Network.Haskoin.Transaction as HT
import qualified Control.Exception.Base as Fail

import qualified Data.ByteString as BS
import           Text.Printf        (printf)

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

