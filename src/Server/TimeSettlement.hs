module Server.TimeSettlement where

-- Time-based settlement
-- Close channels before the channel expiration date

import           Server.ChanStore.Types (ChannelMap)
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

startSettlementThread m i conf =
    putStrLn "Started settlement thread." >> settlementThread m i conf

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
    let settlePeriodSecs = fromInteger $ 3600 * fromIntegral (confSettlePeriod settleConf)
    let settlementTime = settlePeriodSecs `addUTCTime` now

    expiringChannelKeys <- atomically $ channelsExpiringBefore settlementTime m
    forM expiringChannelKeys (settleSingleChannel m (settleConf, rpcInfo))

    return ()

settleSingleChannel ::
    ChannelMap
    -> (ChanSettleConfig, BTCRPCInfo)
    -> HT.OutPoint
    -> IO ()
settleSingleChannel m (settleConf, rpcInfo) k = do
    maybeState <- markAsSettlingAndGetIfOpen m k
    _ <- case maybeState of
        Nothing        -> putStrLn $
            "INFO: Settlement thread: Channel closed inbetween fetching keys and items" ++
            " (this should happen rarely)"
        Just chanState -> settleChannel rpcInfo settleConf chanState >> undefined

    return ()

--     HC.PrvKey
--     -> HC.Address
--     -> BitcoinAmount
--     -> (HT.Tx -> IO (Either String HT.TxHash))
