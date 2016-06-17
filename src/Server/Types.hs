module Server.Types where

import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)
import qualified Network.Haskoin.Crypto as HC


data OpenConfig = OpenConfig Int BitcoinAmount Bool

data ChanSettleConfig = SettleConfig {
    confSettlePrivKey     :: HC.PrvKey,
    confSettleRecvAddr    :: HC.Address,
    confSettleTxFee       :: BitcoinAmount,
    confSettlePeriod      :: Int
}
