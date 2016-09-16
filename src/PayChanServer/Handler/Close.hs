module PayChanServer.Handler.Close where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf

import qualified PayChanServer.DB as DB
import           ChanStore.Interface  as DBConn



chanSettleHandler ::
    SendPubKey
    -> BitcoinLockTime
    -> TxHash
    -> Vout
    -> Maybe Signature
    -> AppPC PaymentResult
chanSettleHandler _ _ _ _ Nothing =
    userError' "Missing payment. \"payment\" query arg should contain most recent channel payment."
chanSettleHandler sendPK lockTime fundTxId fundIdx (Just clientSig) = do
    dbConn <- view Conf.dbInterface
    let chanId = OutPoint fundTxId fundIdx
    chanState <- DB.getChannelStateOr404 dbConn chanId
    -- Authenticate. Most recent channel payment is supplied by client as a token.
    let confirmClientSig storedPayment =
            unless (clientSig == pGetSig storedPayment) $
                userError' "Invalid payment. Provide most recent channel payment."
    -- Decide what to do based on channel state: open/half-open/closed
    (settlementTxId,chanValLeft) <- case chanState of
            (DBConn.ReadyForPayment rpc) -> do
                confirmClientSig $ getNewestPayment rpc
                settleChannel <- view Conf.settleChannel
                settleTxId <- liftIO (settleChannel rpc)
                return (settleTxId, channelValueLeft rpc)
            (DBConn.SettlementInProgress _) ->
                errorWithDescription 410 "Channel is being closed"
            (DBConn.ChannelSettled settleTxId _ rpc) -> do
                confirmClientSig $ getNewestPayment rpc
                return (settleTxId, channelValueLeft rpc)
    -- Write response
    return PaymentResult
           { paymentResult_channel_status     = ChannelClosed
           , paymentResult_channel_valueLeft  = chanValLeft
           , paymentResult_value_received     = 0
           , paymentResult_settlement_txid    = Just settlementTxId
           }
