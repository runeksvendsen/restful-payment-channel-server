module PayChanServer.Handler.Pay where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import qualified PayChanServer.DB as DB
import           ChanStore.Interface  as DBConn


chanPayHandler ::
    SendPubKey
    -> BitcoinLockTime
    -> TxHash
    -> Vout
    -> FullPayment
    -> AppPC PaymentResult
chanPayHandler sendPK lockTime fundTxId fundIdx payment = do
    dbConn <- view Conf.dbInterface
    let chanId = OutPoint fundTxId fundIdx
    existingChanState <- DB.getChannelStateForPayment dbConn chanId

    (valRecvd,newChanState) <- either
            (userError' . show)
            return
            (recvPayment existingChanState payment)

    DB.tryDBRequest (DBConn.chanUpdate dbConn chanId (fpPayment payment)) >>=
        \res -> when (res == WasNotUpdated) $
            errorWithDescription 410 "Channel closed or being closed"

    return PaymentResult
               { paymentResult_channel_status     = ChannelOpen
               , paymentResult_channel_valueLeft  = channelValueLeft newChanState
               , paymentResult_value_received     = valRecvd
               , paymentResult_settlement_txid    = Nothing
               }
