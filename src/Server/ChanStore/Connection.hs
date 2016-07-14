module Server.ChanStore.Connection where

import           Server.ChanStore.Types
import Server.ChanStore.ChanStore
-- import Server.Config.Types

import Control.Concurrent.STM (STM, atomically, throwSTM)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Network.Haskoin.Transaction as HT
import qualified Control.Exception.Base as Fail

import qualified Data.ByteString as BS
import           Text.Printf        (printf)

import           Network.HTTP.Client




isOpen :: Connection -> Bool
isOpen (OpenConnection _) = True
isOpen ClosedConnection   = False

newChanMapConnection :: BS.ByteString -> Word -> IO ChanMapConn
newChanMapConnection host port =
    Conn host port <$> newManager
        defaultManagerSettings {
            managerConnCount = 100
        }

dummyRequest :: IO Request
dummyRequest = parseUrl "http://dummy.com/"

getBaseRequest :: ChanMapConn -> IO Request
getBaseRequest (Conn host port _) =
    dummyRequest >>= \req ->
        return $ req {
            secure = False,
            host = host,
            port = fromIntegral port
        }



-- closeConnection :: ChanMapConn -> IO ()
closeConnection = undefined
--     atomically $ TVar.writeTVar conn ClosedConnection

-- failOnConnClosed :: Connection -> STM ChannelMap
-- failOnConnClosed (OpenConnection map) = return map
-- failOnConnClosed ClosedConnection     = throwSTM (Fail.RecSelError "Connection closed")

-- getConnMap :: ChanMapConn -> STM ChannelMap
-- getConnMap conn = TVar.readTVar conn >>= failOnConnClosed