module PayChanServer.Handler.Pay where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import qualified PayChanServer.DB as DB
import           ChanStore.Interface  as DBConn
import           PayChanServer.Callback.Interface as CB
import           ChanStore.Lib.Types (PayRequest(..), PayResult(..), UpdateResult(..))


chanPayHandler ::
    SendPubKey
    -> BitcoinLockTime
    -> TxHash
    -> Vout
    -> FullPayment
    -> AppPC PaymentResult
chanPayHandler sendPK lockTime fundTxId fundIdx payment = do
    -- TODO: verify resource/payment match
    dbConn <- view Conf.dbInterface
    (PaymentReceived valRecvd valLeft chanTotalValue) <-
        DB.tryDBRequest (DBConn.chanPay dbConn sendPK payment) >>=
            \res -> case res of
                pr@PaymentReceived{} -> return pr
                PaymentError err    -> userError' $ show err
                PayUpdateError NoSuchChannel       -> errorWithDescription 404 "No such channel"
                PayUpdateError (ChanClosed _ _)    -> errorWithDescription 410 "Channel closed or being closed"
                PayUpdateError ChanBeingClosed     -> errorWithDescription 410 "Channel closed or being closed"
    -- Contact the content delivery service for application data
    callbackConn <- view Conf.callbackIface
    let payInfo = CB.PaymentInfo valRecvd sendPK valLeft chanTotalValue
    (CB.PaymentResponse payData) <- DB.tryDBRequest (CB.valueReceived callbackConn payInfo)
    return PaymentResult
               { paymentResult_channel_status     = ChannelOpen
               , paymentResult_channel_valueLeft  = valLeft
               , paymentResult_value_received     = valRecvd
               , paymentResult_settlement_txid    = Nothing
               , paymentResult_application_data   = payData
               }
