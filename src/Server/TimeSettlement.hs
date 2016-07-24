module Server.TimeSettlement where

-- Time-based settlement
-- Close channels before the channel expiration date

import           Server.ChanStore.Types (ChannelMap)
import           Server.ChanStore.ChanStore (channelsExpiringBefore, markAsSettlingAndGetIfOpen)
-- import           Server.ChanStore.Settlement (settleChannelEither)
import           Server.Types (ServerSettleConfig(..))
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount)
import           Bitcoind (BTCRPCInfo, bitcoindNetworkSumbitTx)

import           SigningService.Interface (settleChannel)
import           Server.DB (trySigningRequest)


import qualified Network.Haskoin.Transaction as HT
import           Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import           Control.Concurrent.STM (STM, atomically, throwSTM)
import           Control.Monad.Catch (bracket, finally, try)
import           Control.Concurrent  (forkIO, killThread, threadDelay)
import           Control.Monad (forM)
import           Data.Maybe (isJust, isNothing, fromJust)


startSettlementThread m i conf =
    putStrLn "Started settlement thread." >> settlementThread m i conf

settlementThread ::
    ChannelMap
    -> Int -- ^ Check interval in seconds
    -> (ServerSettleConfig, BTCRPCInfo)
    -> IO ()
settlementThread m delaySecs (settleConf, rpcInfo) =
    (threadDelay delayMicroSecs >> settleExpiringChannels m delaySecs (settleConf, rpcInfo))
        `finally` settlementThread m delaySecs (settleConf, rpcInfo)
            where delayMicroSecs = delaySecs * round 1e6

settleExpiringChannels ::
    ChannelMap
    -> Int -- ^ Check interval in seconds
    -> (ServerSettleConfig, BTCRPCInfo)
    -> IO ()
settleExpiringChannels m delaySecs (settleConf,rpcInfo) = do
    now <- getCurrentTime
    let settlePeriodSecsOffset = fromInteger $
            (fromIntegral delaySecs) + (-3600 * fromIntegral (confSettlePeriod settleConf))
    let settlementTimeCutoff = settlePeriodSecsOffset `addUTCTime` now
    expiringChannelKeys <- atomically $ channelsExpiringBefore settlementTimeCutoff m


    -- let chansKeysToSettle = [fromJust s | s <- maybeChanKeysToSettle, isJust s]
    return ()

-- _settleConfig    :: ServerSettleConfig
--  , _signSettleFunc  :: ReceiverPaymentChannel -> IO HT.Tx
--  , _pushTxFunc      :: HT.Tx -> IO (Either String HT.TxHash)


settleSingleChannel ::
    HT.OutPoint
    -> (ReceiverPaymentChannel -> IO HT.Tx)
    -> (HT.Tx -> IO (Either String HT.TxHash))
    -> BitcoinAmount
    -> IO (Either String HT.TxHash)
settleSingleChannel chanId signFunc pushTxFunc fee = do
    maybeKey <- atomically $ markAsSettlingAndGetIfOpen m key
    case maybeKey of
        Nothing        -> putStrLn $
            "INFO: Settlement thread: Channel closed (by other thread) inbetween fetching key and item" ++
            " (this should happen rarely)"  -- LOGINFO
        Just chanState ->
            trySigningRequest (settleChannel m chanState txFee) >>=
            bitcoindNetworkSumbitTx rpcInfo

