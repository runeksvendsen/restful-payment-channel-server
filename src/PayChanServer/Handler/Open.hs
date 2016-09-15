module PayChanServer.Handler.Open where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import           Data.Bitcoin.PaymentChannel        (channelFromInitialPayment)

import qualified PayChanServer.DB as DB
import qualified ChanStore.Interface as DBConn

import           Control.Monad.IO.Class (liftIO)

chanOpenHandler ::
    SendPubKey
    -> BitcoinLockTime
    -> TxHash
    -> Vout
    -> Payment
    -> AppPC PaymentResult
chanOpenHandler sendPK lockTime fundTxId fundIdx payData = do
    dustLimit <- Conf.dustLimit <$> view Conf.chanConf
    pubKeyServ <- view Conf.pubKey
    let cp = CChannelParameters sendPK pubKeyServ lockTime (Conf.getVal dustLimit)
    -- TODO: check resource txId/vout match
    fundingTxInfo <- blockchainGetConfirmedTxInfo cp
    dbConn <- view Conf.dbInterface

    let payment = payData
    (valRecvd,recvChanState) <- either (userError' . show) return $
        channelFromInitialPayment cp fundingTxInfo (getFundingAddress cp) payment

    openPrice <- Conf.openPrice <$> view Conf.chanConf
    when (valRecvd < openPrice) $
        userError' $ "Initial payment short: open price is " ++
            show openPrice ++ ", received " ++ show valRecvd

    DB.tryDBRequest (DBConn.chanAdd dbConn recvChanState) >>=
        \res -> when (res == AlreadyExists) $
            errorWithDescription 409 "Channel already exists"

    return PaymentResult
           { paymentResultChannelStatus     = ChannelOpen
           , paymentResultChannelValueLeft  = channelValueLeft recvChanState
           , paymentResultValueReceived     = valRecvd
           , paymentResultSettlementTxid    = Nothing
           }

