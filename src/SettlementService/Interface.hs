module SettlementService.Interface
(
    settleChannel
)
where

import           SettlementService.Spec (SettleChan(..))
import           Server.ChanStore.Connection (ConnManager)
import           Server.ChanStore.RequestRunner (runRequest)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel)
-- import qualified Network.Haskoin.Transaction as HT

settleChannel :: ConnManager -> ReceiverPaymentChannel -> IO ()
settleChannel conn rpc = runRequest conn $ SettleChan rpc
