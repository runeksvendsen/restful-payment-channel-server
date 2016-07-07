module Server.ChanStore.Expiration where

import Server.ChanStore.ChanStore

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel)
import qualified ListT as LT
import qualified STMContainers.Map as Map
import           Control.Concurrent.STM (STM, atomically, throwSTM)
-- import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
-- import           Data.Time.Clock (getCurrentTime)
-- --DEBUG
-- import           DiskStore (DiskMap, getFilteredItems)
-- import Network.Haskoin.Transaction as HT
-- --DEBUG
--
-- getExpiringChannels :: Word -> DiskMap HT.TxHash ChanState -> IO [ReceiverPaymentChannel]
-- getExpiringChannels headstartHours m = do
--     currentTime <- getCurrentTime
--     atomically $ map csState . filter (not . isSettled) getFilteredItems m
