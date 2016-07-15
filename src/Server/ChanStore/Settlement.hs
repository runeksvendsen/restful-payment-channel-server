module Server.ChanStore.Settlement where --TODO: move

import           Server.ChanStore.Types
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless, when)
import           Snap (MonadSnap)

import           Server.Types (ChanSettleConfig(..))
import           Server.Util (internalError)
import           Bitcoind (BTCRPCInfo, bitcoindNetworkSumbitTx)

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft)
import           Data.Bitcoin.PaymentChannel.Util (setSenderChangeAddress, BitcoinLockTime)

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT


settleChannel :: MonadSnap m =>
    BTCRPCInfo
    -> ChanSettleConfig
    -> ReceiverPaymentChannel
    -> m HT.TxHash
settleChannel rpc conf s = either internalError return =<< liftIO (settleChannelEither rpc conf s)

settleChannelEither ::
    BTCRPCInfo
    -> ChanSettleConfig
    -> ReceiverPaymentChannel
    -> IO (Either String HT.TxHash)
settleChannelEither rpcInfo (SettleConfig privKey recvAddr txFee _) chanState =
    either (return . Left . show) pushTx $
        getSettlementBitcoinTx chanState (`HC.signMsg` privKey) recvAddr txFee
            where pushTx = bitcoindNetworkSumbitTx rpcInfo


