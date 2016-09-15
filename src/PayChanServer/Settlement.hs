{-|
Deals with settling channels. Both channels for expiry
(by querying the ChanStore) and channels that are settled either
because the client requested it or
-}

module  PayChanServer.Settlement
(
    settleChannel,
    settlementThread
)

where


import           PayChanServer.Config.Types as Conf (ServerSettleConfig(..), SettleHours, getVal)
import           PayChanServer.DB (tryHTTPRequestOfType)

import           ChanStore.Interface  as DBConn

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount,
                                                    PaymentChannel(getChannelID))
import           SigningService.Interface (signSettlementTx)

import qualified Network.Haskoin.Transaction as HT
import           Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import           Control.Monad.Catch (handleAll)
import           Control.Concurrent as Concurrent
import           Control.Monad (forM)
import qualified BlockchainAPI.Impl.Bitcoind.Interface as Btc

-- TODO: Figure out how to alert the server operator of important errors
-- Send email? (first step implementation: PayChanServer.Misc.Email)
logImportantError = putStrLn
logImportantErrorThenFail e = logImportantError e >> error e

-- | Fully settle a given payment channel; return settlement txid
settleChannel ::
    DBConn.Interface
    -> ConnManager
    -> Btc.Interface
    -> BitcoinAmount
    -> ReceiverPaymentChannel
    -> IO HT.TxHash
settleChannel dbIface signConn rpcInfo txFee rpc =
    tryRequest "BeginSettle" (DBConn.settleByIdBegin dbIface (getChannelID rpc))
        >>= finishSettleChannel dbIface signConn rpcInfo txFee

-- | Thread that queries the ChanStore for channels close to expiriry,
--   and settles them
settlementThread ::
    DBConn.Interface
    -> ConnManager
    -> ServerSettleConfig
    -> Btc.Interface
    -> Int -- ^ Check interval in seconds
    -> IO ()
settlementThread dbIface signConn settleConf btcIface delaySecs = do
    let delayMicroSecs = delaySecs * round 1e6
    threadDelay delayMicroSecs

    forkIO $ settlementThread dbIface signConn settleConf btcIface delaySecs
    handleAll  -- Log uncaught errors from settling channels
        ( logImportantError . ("Error in settlement thread (needs attention): " ++) . show )
        ( settleExpiringChannels dbIface signConn settleConf btcIface >>=
          mapM_ ( putStrLn . ("Settled channel: " ++) . show )
        )


-- | After getting the channel state (ReceiverPaymentChannel) from the DB while
--   simultaneously marking it as being closed, this function is used
--   to produce and publish the settlement tx.
--  First the SettlementService creates and signs the tx, then we submit this to
--   the Bitcoin network, and finally mark the channel as completely closed
--   in the ChanStore.
finishSettleChannel ::
    DBConn.Interface
    -> ConnManager
    -> Btc.Interface
    -> BitcoinAmount
    -> ReceiverPaymentChannel
    -> IO HT.TxHash
finishSettleChannel dbIface signConn btcIface txFee rpc = do
    settlementTx   <- tryRequest "Signing" (signSettlementTx signConn txFee rpc)
    settlementTxId <- tryBitcoind =<< Btc.publishTx btcIface settlementTx -- HEY
    tryRequest "FinishSettle" (DBConn.settleFin dbIface (getChannelID rpc) settlementTxId)
    return settlementTxId
        where tryBitcoind = either logImportantErrorThenFail return


--- Time-based settlement
settleExpiringChannels ::
    DBConn.Interface
    -> ConnManager
    -> ServerSettleConfig
    -> Btc.Interface
    -> IO [HT.TxHash]
settleExpiringChannels dbIface signConn (Conf.ServerSettleConfig txFee settlePeriod) btcIface = do
    settlementTimeCutoff <- getExpirationDateCutoff settlePeriod
    expiringChannels <- tryRequest "BeginSettle" (DBConn.settleByExpBegin dbIface settlementTimeCutoff)
    forM expiringChannels (finishSettleChannel dbIface signConn btcIface txFee)

getExpirationDateCutoff :: Conf.SettleHours -> IO UTCTime
getExpirationDateCutoff settlePeriodHours = do
    now <- getCurrentTime
    -- Head start. We want to close channels a number of hours *before* the actual expiration date,
    --  to be on the safe side.
    let settlePeriodSecsOffset = fromInteger $ 3600 * fromIntegral (Conf.getVal settlePeriodHours)
    -- So we get the current time, add to it our head start, and get all channels
    --  expiring *before* the resulting date.
    return $ settlePeriodSecsOffset `addUTCTime` now


tryRequest typeStr req = either logImportantErrorThenFail return =<<
                         tryHTTPRequestOfType typeStr req

