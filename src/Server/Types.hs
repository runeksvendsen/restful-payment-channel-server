module Server.Types where

import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount, Payment, BitcoinLockTime)
-- import           Data.Bitcoin.PaymentChannel.Util (BitcoinLockTime)
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT
import           Server.ChanStore (ChannelMap, ChanState(..))
import qualified Data.ByteString as BS
import           BlockchainAPI.Types (TxInfo)

data OpenConfig = OpenConfig Int BitcoinAmount Bool

data ChanSettleConfig = SettleConfig {
    confSettlePrivKey     :: HC.PrvKey,
    confSettleRecvAddr    :: HC.Address,
    confSettleTxFee       :: BitcoinAmount,
    confSettlePeriod      :: Int
}


--- TYPES ---
type Vout = Integer

data ChanOpenConfig = ChanOpenConfig
    BitcoinAmount HC.PubKey ChannelMap TxInfo String BS.ByteString HC.PubKey HC.Address BitcoinLockTime Payment
--    open_price  server_pk                   client_pk

data ChanPayConfig = PayConfig
    ChannelMap HT.TxHash Vout (Maybe HC.Address) Payment



data StdConfig = StdConfig {
    serverChanMap   :: ChannelMap,

    chanHash        :: HT.TxHash,
    chanVout        :: Vout,
    chanPayment     :: Payment
}
