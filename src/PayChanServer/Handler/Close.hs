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
    -> Maybe Payment
    -> AppPC PaymentResult
chanSettleHandler _ _ _ _ Nothing =
    userError' "Missing payment. \"payment\" query arg should contain most recent channel payment."
chanSettleHandler sendPK lockTime fundTxId fundIdx (Just clientPayment) = do
    dbConn <- view Conf.dbInterface
    let chanId = OutPoint fundTxId fundIdx
    chanState <- DB.getChannelStateOr404 dbConn chanId
    -- Authenticate. Most recent channel payment is supplied by client as a token.
    let confirmClientPayment storedPayment =
            unless (clientPayment == storedPayment) $
                userError' "Invalid payment. Provide most recent channel payment."
    -- Decide what to do based on channel state: open/half-open/closed
    (settlementTxId,chanValLeft) <- case chanState of
            (DBConn.ReadyForPayment rpc) -> do
                confirmClientPayment $ getNewestPayment rpc
                settleChannel <- view Conf.settleChannel
                settleTxId <- liftIO (settleChannel rpc)
                return (settleTxId, channelValueLeft rpc)
            (DBConn.SettlementInProgress _) ->
                errorWithDescription 410 "Channel is being closed"
            (DBConn.ChannelSettled settleTxId _ rpc) -> do
                confirmClientPayment $ getNewestPayment rpc
                return (settleTxId, channelValueLeft rpc)
    -- Write response
    return PaymentResult
           { paymentResultChannelStatus     = ChannelClosed
           , paymentResultChannelValueLeft  = chanValLeft
           , paymentResultValueReceived     = 0
           , paymentResultSettlementTxid    = Just settlementTxId
           }