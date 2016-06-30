module Server.Types where

import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount, Payment, BitcoinLockTime)
-- import           Data.Bitcoin.PaymentChannel.Util (BitcoinLockTime)
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT
import           Server.ChanStore (ChannelMap, ChanState(..))
import qualified Data.ByteString as BS
import           BlockchainAPI.Types (TxInfo)


type Vout = Integer     -- Output index


data OpenConfig = OpenConfig Int BitcoinAmount Bool

data ChanSettleConfig = SettleConfig {
    confSettlePrivKey     :: HC.PrvKey,
    confSettleRecvAddr    :: HC.Address,
    confSettleTxFee       :: BitcoinAmount,
    confSettlePeriod      :: Int
}


data ChanOpenConfig = ChanOpenConfig {
    ocOpenPrice     :: BitcoinAmount
   ,ocServerPubKey  :: HC.PubKey
   ,ocChanMap       :: ChannelMap
   ,ocFundingInfo   :: TxInfo
--    ,ocHostname      :: String
   ,ocBasePath      :: BS.ByteString
   ,ocClientPubKey  :: HC.PubKey
   ,ocClientChange  :: HC.Address
   ,ocExpTime       :: BitcoinLockTime
   ,ocInitPayment   :: Payment
}


data ChanPayConfig = PayConfig
    ChannelMap HT.TxHash Vout (Maybe HC.Address) Payment


data StdConfig = StdConfig {
    serverChanMap   :: ChannelMap,

    chanHash        :: HT.TxHash,
    chanVout        :: Vout,
    chanPayment     :: Payment
}
