module Server.ChanStore.Settlement where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless, when)

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft)
import           Data.Bitcoin.PaymentChannel.Util (setSenderChangeAddress, BitcoinLockTime)

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT


settleChannel ::
    HC.PrvKey
    -> HC.Address
    -> BitcoinAmount
    -> (HT.Tx -> IO (Either String HT.TxHash))
    -> ReceiverPaymentChannel
    -> IO (Either String HT.TxHash)
settleChannel privKey recvAddr txFee pushTx chanState =
    either (return . Left . show) pushTx $
        getSettlementBitcoinTx chanState (`HC.signMsg` privKey) recvAddr txFee

