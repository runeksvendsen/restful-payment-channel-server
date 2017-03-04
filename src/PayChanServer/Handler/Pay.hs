module PayChanServer.Handler.Pay where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import qualified RBPCP.Types as RBPCP
import qualified PayChanServer.DB as DB
import           ChanStore.Interface  as DBConn
import           PayChanServer.Callback.Interface as CB
import           AppPrelude.Man (PayBeginRequest(..), PayBeginResult(..), UpdateResult(..))


{-

data CallbackInfo = CallbackInfo
  { amount              :: BtcAmount
  , chan_value_left     :: BtcAmount
  , chan_total_value    :: BtcAmount
  , client_app_data     :: T.Text
  , full_payment        :: SignedPayment
  } deriving (Generic, FromJSON, ToJSON)

data CallbackResponse = CallbackResponse
  { resp_app_data       :: T.Text
  , resp_app_error      :: Maybe T.Text
  } deriving (Generic, FromJSON, ToJSON)

-}

chanPayHandler ::
    SendPubKey
    -> LockTimeDate
    -> TxHash
    -> Vout
    -> RBPCP.Payment
    -> AppPC PaymentResult
chanPayHandler sendPK lockTime fundTxId fundIdx (RBPCP.Payment payment appData) = do
    -- TODO: verify resource/payment match
    dbConn <- view Conf.dbInterface
    -- TODO: Payment handle
    let valRecvd = undefined
        valLeft = undefined
        chanTotalValue = undefined
    -- Contact the content delivery service for application response data
    callbackConn <- view Conf.callbackIface
    let payInfo = CB.CallbackInfo valRecvd valLeft chanTotalValue appData payment
    (CB.CallbackResponse payData errM) <- DB.tryDBRequest (CB.valueReceived callbackConn payInfo)
    return PaymentResult
               { paymentResult_channel_status     = ChannelOpen
               , paymentResult_channel_valueLeft  = valLeft
               , paymentResult_value_received     = valRecvd
               , paymentResult_settlement_txid    = Nothing
               , paymentResult_application_data   = payData
               }
