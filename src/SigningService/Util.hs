module SigningService.Util where


import           PayChanServer.Types (SigningSettleConfig(..))
import           Common.Util (internalError)

import           Data.Bitcoin.PaymentChannel (getSettlementBitcoinTx)
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    BitcoinLockTime(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft, PayChanError)


import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

-- |Used by SigningService
produceSettlementTx ::
    SigningSettleConfig
    -> (ReceiverPaymentChannel, BitcoinAmount)
    -> HT.Tx
produceSettlementTx (SigningSettleConfig privKey recvAddr) (chanState,txFee) =
    getSettlementBitcoinTx chanState (`HC.signMsg` privKey) recvAddr txFee
