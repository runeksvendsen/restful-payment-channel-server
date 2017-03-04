module PayChanServer.Handler.Open where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import qualified RBPCP.Types as RBPCP
import           PaymentChannel        (channelFromInitialPayment)

import qualified PayChanServer.DB as DB
import qualified ChanStore.Interface as DBConn
import           AppPrelude.Man

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT


-- TODO: Deprecated (probably)
tmpMkExtended :: ChanParams -> FundingTxInfo -> RecvPayChan -> ServerPayChanX
tmpMkExtended cp fti rpc = do
    let fakeXPub = HC.XPubKey 0 0 0 (HT.getTxHash $ ftiHash fti) (getPubKey $ cpReceiverPubKey cp)
    fromMaybe (error "mkExtendedKeyRPC failed") $ mkExtendedKeyRPC rpc fakeXPub



chanOpenHandler ::
    SendPubKey
    -> LockTimeDate
    -> TxHash
    -> Vout
    -> RBPCP.Payment
    -> AppPC PaymentResult
chanOpenHandler sendPK lockTime fundTxId fundIdx (RBPCP.Payment payment appData) = do
    dbConn <- view Conf.dbInterface

    let cp = MkChanParams sendPK (getRecvPubKey payment) lockTime
    let redirect = error "STUB" -- TODO: Redirect
    fti@(CFundingTxInfo hash idx _) <- blockchainGetConfirmedTxInfo cp
    when (fundTxId /= hash || fundIdx /= idx) $
        redirect "/ hash idx"

    -- Create channel state
    srvConf <- view Conf.chanConf
    now <- liftIO getCurrentTime
    (valRecvd,rpc) <- either (userError' . show) return $
        channelFromInitialPayment cp fti payment

    openPrice <- Conf.getVal . Conf.openPrice <$> view Conf.chanConf
    when (valRecvd < openPrice) $
        userError' $ "Initial payment short: open price is " ++
            show openPrice ++ ", received " ++ show valRecvd

    let openReq = OpenRequest $ tmpMkExtended cp fti rpc
    _ <- DB.tryDBRequest (DBConn.chanOpen dbConn openReq) >>=
        \res -> case res of
            ChannelCreated -> return ()
            ChannelExists          ->
                errorWithDescription 409 "Channel already exists for pubkey"

    return PaymentResult
           { paymentResult_channel_status     = ChannelOpen
           , paymentResult_channel_valueLeft  = channelValueLeft rpc
           , paymentResult_value_received     = valRecvd
           , paymentResult_settlement_txid    = Nothing
           , paymentResult_application_data   = ""
           }
