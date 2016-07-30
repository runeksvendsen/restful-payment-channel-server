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


import           PayChanServer.Types (ServerSettleConfig(..))
import           PayChanServer.DB (tryHTTPRequestOfType)

import           ChanStore.Interface  as DBConn

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount,
                                                    PaymentChannel(getChannelID))
import           Bitcoind (BTCRPCInfo, bitcoindNetworkSumbitTx)
import           SigningService.Interface (signSettlementTx)

import qualified Network.Haskoin.Transaction as HT
import           Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import           Control.Monad.Catch (finally, handleAll)
import           Control.Concurrent  (threadDelay)
import           Control.Monad (forM)


-- TODO: Figure out how to alert the server operator of important errors
-- Send email? (first step implementation: PayChanServer.Misc.Email)
logImportantError = putStrLn
logImportantErrorThenFail e = logImportantError e >> error e

-- | Fully settle a given payment channel; return settlement txid
settleChannel ::
    ConnManager
    -> ConnManager
    -> BTCRPCInfo
    -> BitcoinAmount
    -> ReceiverPaymentChannel
    -> IO HT.TxHash
settleChannel dbConn signConn rpcInfo txFee rpc =
    tryRequest "BeginSettle" (DBConn.settleByIdBegin dbConn (getChannelID rpc))
        >>= finishSettleChannel dbConn signConn rpcInfo txFee

-- | Thread that queries the ChanStore for channels close to expiriry,
--   and settles them
settlementThread ::
    ConnManager
    -> ConnManager
    -> ServerSettleConfig
    -> BTCRPCInfo
    -> Int -- ^ Check interval in seconds
    -> IO ()
settlementThread dbConn signConn settleConf rpcInfo delaySecs =
    let delayMicroSecs = delaySecs * round 1e6 in
    (handleAll  -- Log uncaught errors from settling channels
        ( logImportantError . ("Error in settlement thread (needs attention): " ++) . show )
        ( threadDelay delayMicroSecs >>
          settleExpiringChannels dbConn signConn (settleConf, rpcInfo) >>=
          (mapM ( putStrLn . ("Settled channel: " ++) . show ) ) >>
          return ()
        )
    )
    `finally` (    -- Regardless of what happens, keep thread running
        settlementThread dbConn signConn settleConf rpcInfo delaySecs )

-- | After getting the channel state (ReceiverPaymentChannel) from the DB while
--   simultaneously marking it as being closed, this function is used
--   to produce and publish the settlement tx.
--  First the SettlementService creates and signs the tx, then we submit this to
--   the Bitcoin network, and finally mark the channel as completely closed
--   in the ChanStore.
finishSettleChannel ::
    ConnManager
    -> ConnManager
    -> BTCRPCInfo
    -> BitcoinAmount
    -> ReceiverPaymentChannel
    -> IO HT.TxHash
finishSettleChannel dbConn signConn rpcInfo txFee rpc = do
    settlementTx   <- tryRequest "Signing" (signSettlementTx signConn txFee rpc)
    settlementTxId <- tryBitcoind =<< bitcoindNetworkSumbitTx rpcInfo settlementTx -- HEY
    tryRequest "FinishSettle" (DBConn.settleFin dbConn (getChannelID rpc) settlementTxId)
    return settlementTxId
        where tryBitcoind = either logImportantErrorThenFail return


--- Time-based settlement
settleExpiringChannels ::
    ConnManager
    -> ConnManager
    -> (ServerSettleConfig, BTCRPCInfo)
    -> IO [HT.TxHash]
settleExpiringChannels dbConn signConn (ServerSettleConfig txFee settlePeriod,rpcInfo) = do
    settlementTimeCutoff <- getExpirationDateCutoff settlePeriod
    expiringChannels <- tryRequest "BeginSettle" (DBConn.settleByExpBegin dbConn settlementTimeCutoff)
    forM expiringChannels (finishSettleChannel dbConn signConn rpcInfo txFee)

getExpirationDateCutoff :: Int -> IO UTCTime
getExpirationDateCutoff settlePeriodHours = do
    now <- getCurrentTime
    -- Head start. We want to close channels a number of hours *before* the actual expiration date,
    --  to be on the safe side.
    let settlePeriodSecsOffset = fromInteger $ 3600 * fromIntegral settlePeriodHours
    -- So we get the current time, add to it our head start, and get all channels
    --  expiring *before* the resulting date.
    return $ settlePeriodSecsOffset `addUTCTime` now


tryRequest typeStr req = either logImportantErrorThenFail return =<<
                         tryHTTPRequestOfType typeStr req

