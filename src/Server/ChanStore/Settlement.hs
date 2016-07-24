module Server.ChanStore.Settlement where --TODO: move

import           Server.ChanStore.Types
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless, when)
import           Snap (MonadSnap)

import           Server.Types (ServerSettleConfig(..), SigningSettleConfig(..))
import           Server.Util (internalError)
import           Bitcoind (BTCRPCInfo, bitcoindNetworkSumbitTx)

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft, PayChanError)
import           Data.Bitcoin.PaymentChannel.Util (setSenderChangeAddress, BitcoinLockTime)

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT




-- settleChannelEither ::
--     BTCRPCInfo
--     -> ConnManager
--     -> [ReceiverPaymentChannel]
--     -> IO (Either String HT.TxHash)
-- settleChannelEither rpcInfo conn chanStates =
--     either (return . Left . show) pushTx $
--         trySigningRequest (settleChannel conn chanStates)
--             where pushTx = bitcoindNetworkSumbitTx rpcInfo

-- |Used by SigningService
produceSettlementTx ::
    SigningSettleConfig
    -> (ReceiverPaymentChannel, BitcoinAmount)
    -> Either PayChanError HT.Tx
produceSettlementTx (SigningSettleConfig privKey recvAddr) (chanState,txFee) =
    getSettlementBitcoinTx chanState (`HC.signMsg` privKey) recvAddr txFee
