module Server.ChanStore.Connection where

import Server.ChanStore.ChanStore

import Control.Concurrent.STM (STM, atomically, throwSTM)
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Network.Haskoin.Transaction as HT
import qualified Control.Exception.Base as Fail

data Connection =
    OpenConnection ChannelMap |
    ClosedConnection

isOpen :: Connection -> Bool
isOpen (OpenConnection _) = True
isOpen ClosedConnection   = False

type ChanMapConn = TVar.TVar Connection

newChanMapConnection :: ChannelMap -> IO ChanMapConn
newChanMapConnection map =
    TVar.newTVarIO $ OpenConnection map

closeConnection :: ChanMapConn -> IO ()
closeConnection conn =
    atomically $ TVar.writeTVar conn ClosedConnection

failOnConnClosed :: Connection -> STM ChannelMap
failOnConnClosed (OpenConnection map) = return map
failOnConnClosed ClosedConnection     = throwSTM (Fail.RecSelError "Connection closed")

getConnMap :: ChanMapConn -> STM ChannelMap
getConnMap conn = TVar.readTVar conn >>= failOnConnClosed