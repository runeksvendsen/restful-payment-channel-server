module Server.TimeSettlement where

-- Time-based settlement
-- Close channels before the channel expiration date

import           Server.ChanStore (ChannelMap)
import           Server.ChanStore.ChanStore (channelsExpiringBefore, markAsSettlingAndGetIfOpen)
import           Server.ChanStore.Settlement (settleChannel)
import           Server.Types (ChanSettleConfig(..))
import           Bitcoind (BTCRPCInfo)

import qualified Network.Haskoin.Transaction as HT
import           Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import           Control.Concurrent.STM (STM, atomically, throwSTM)
import           Control.Monad.Catch (bracket, finally, try)
import           Control.Concurrent  (forkIO, killThread, threadDelay)
import           Control.Monad (forM)

startSettlementThread = (>>) (putStrLn "Started disk sync thread.") settlementThread

settlementThread ::
    ChannelMap
    -> Int -- ^ Check interval in seconds
    -> (ChanSettleConfig, BTCRPCInfo)
    -> IO ()
settlementThread m delaySecs (settleConf, rpcInfo) =
    (threadDelay delayMicroSecs >> settleExpiringChannels m (settleConf, rpcInfo))
        `finally` settlementThread m delaySecs (settleConf, rpcInfo)
            where delayMicroSecs = delaySecs * round 1e6

settleExpiringChannels ::
    ChannelMap
    -> (ChanSettleConfig, BTCRPCInfo)
    -> IO ()
settleExpiringChannels m (settleConf, rpcInfo) = do
    now <- getCurrentTime
    let settlementTime =  settlePeriodSecs `addUTCTime` now
    expiringChannelKeys <- atomically $ channelsExpiringBefore settlementTime m
    forM expiringChannelKeys (settleSingleChannel m (settleConf, rpcInfo))
        where settlePeriodSecs = fromInteger $ 3600 * fromIntegral (confSettlePeriod settleConf)

settleSingleChannel ::
    ChannelMap
    -> (ChanSettleConfig, BTCRPCInfo)
    -> HT.TxHash
    -> IO ()
settleSingleChannel m (settleConf, rpcInfo) k = do
    maybeState <- markAsSettlingAndGetIfOpen m k
    case maybeState of
        Nothing        -> putStrLn $
            "INFO: Settlement thread: Channel closed inbetween fetching keys and items" ++
            " (this should happen rarely)"
        Just chanState -> settleChannel settleConf rpcInfo chanState

    return ()

--     HC.PrvKey
--     -> HC.Address
--     -> BitcoinAmount
--     -> (HT.Tx -> IO (Either String HT.TxHash))
