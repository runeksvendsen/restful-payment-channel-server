module PayChanServer.Handler.Open where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import           Data.Bitcoin.PaymentChannel        (channelFromInitialPayment)

import qualified PayChanServer.DB as DB
import qualified ChanStore.Interface as DBConn
import           ChanStore.Lib.Types

import           Control.Monad.IO.Class (liftIO)

chanOpenHandler ::
    SendPubKey
    -> BitcoinLockTime
    -> TxHash
    -> Vout
    -> FullPayment
    -> AppPC PaymentResult
chanOpenHandler sendPK lockTime fundTxId fundIdx payment = do
    dustLimit <- Conf.dustLimit <$> view Conf.chanConf
    pubKeyServ <- view Conf.pubKey
    let cp = CChannelParameters sendPK pubKeyServ lockTime (Conf.getVal dustLimit)
    -- TODO: check payment/resource parameter match
    fundingTxInfo <- blockchainGetConfirmedTxInfo cp
    dbConn <- view Conf.dbInterface

    let openReq = OpenRequest cp fundingTxInfo payment
    (valRecvd,chanValLeft) <- DB.tryDBRequest (DBConn.chanOpen dbConn openReq) >>=
        \res -> case res of
            ChannelOpened amtRecvd valLeft -> return (amtRecvd,valLeft)
            OpenError     err      -> userError' $ show err
            ChannelExists          ->
                errorWithDescription 409 "Channel already exists"

    openPrice <- Conf.getVal . Conf.openPrice <$> view Conf.chanConf
    when (valRecvd < openPrice) $
        userError' $ "Initial payment short: open price is " ++
            show openPrice ++ ", received " ++ show valRecvd

    return PaymentResult
           { paymentResult_channel_status     = ChannelOpen
           , paymentResult_channel_valueLeft  = chanValLeft
           , paymentResult_value_received     = valRecvd
           , paymentResult_settlement_txid    = Nothing
           }
