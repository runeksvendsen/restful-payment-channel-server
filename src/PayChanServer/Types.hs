module  PayChanServer.Types where

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount,
                                                    Payment, BitcoinLockTime, FundingTxInfo,
                                                    SendPubKey, RecvPubKey)
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT

-- import           ConnManager.Types (ConnManager)
import           ChanStore.Interface  as DBConn
import qualified Data.ByteString as BS



data StdConfig = StdConfig {
    serverChanMap   :: DBConn.Interface,
    chanId          :: HT.OutPoint,
    chanPayment     :: Payment
}

data OpenHandlerConf = OpenHandlerConf {
    ocOpenPrice     :: BitcoinAmount
   ,ocServerPubKey  :: RecvPubKey
   ,ocDBConn        :: DBConn.Interface
   ,ocFundingInfo   :: FundingTxInfo
   ,ocClientPubKey  :: SendPubKey
   ,ocClientChange  :: HC.Address
   ,ocExpTime       :: BitcoinLockTime
   ,ocInitPayment   :: Payment
}

data ChanPayConfig = PayConfig
    StdConfig (Maybe HC.Address)

data ServerSettleConfig = ServerSettleConfig {
    confSettleTxFee       :: BitcoinAmount,
    confSettlePeriod      :: Int
}

data SigningSettleConfig = SigningSettleConfig {
    confSettlePrivKey     :: HC.PrvKey,
    confSettleRecvAddr    :: HC.Address
}

