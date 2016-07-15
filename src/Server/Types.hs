module Server.Types where

import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount, Payment, BitcoinLockTime)
-- import           Data.Bitcoin.PaymentChannel.Util (BitcoinLockTime)
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT

-- import           Server.ChanStore (ChannelMap)
import           Server.ChanStore.Types (ConnManager)
import qualified Data.ByteString as BS
import           BlockchainAPI.Types (TxInfo)


type Vout = Integer     -- Output index

data StdConfig = StdConfig {
    serverChanMap   :: ConnManager,
    chanId          :: HT.OutPoint,
    chanPayment     :: Payment
}

data ChanOpenConfig = ChanOpenConfig {
    ocOpenPrice     :: BitcoinAmount
   ,ocServerPubKey  :: HC.PubKey
   ,ocChanMap       :: ConnManager
   ,ocFundingInfo   :: TxInfo
   ,ocBasePath      :: BS.ByteString
   ,ocClientPubKey  :: HC.PubKey
   ,ocClientChange  :: HC.Address
   ,ocExpTime       :: BitcoinLockTime
   ,ocInitPayment   :: Payment
}

data OpenConfig = OpenConfig Int BitcoinAmount Bool

data ChanPayConfig = PayConfig
    StdConfig (Maybe HC.Address)

data ChanSettleConfig = SettleConfig {
    confSettlePrivKey     :: HC.PrvKey,
    confSettleRecvAddr    :: HC.Address,
    confSettleTxFee       :: BitcoinAmount,
    confSettlePeriod      :: Int
}

