module Server.ChanStore.Types where

import           DiskStore

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment)
import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString as BS
import           Network.HTTP.Client (Manager)


data Connection =
    OpenConnection ChannelMap |
    ClosedConnection

type Host = BS.ByteString
type Port = Word

data ChanMapConn = Conn Host Port Manager


-- |Holds state for payment channel
data ChanState =
    ReadyForPayment {
        csState             :: ReceiverPaymentChannel
    } |
    ChannelSettled {
        csSettlementTxId    :: HT.TxHash
    } |
    SettlementInProgress {
        csSettlingState     :: ReceiverPaymentChannel
    }

-- Needed for Binary instance non-overlap
newtype MaybeChanState = MaybeChanState (Maybe ChanState)

type Key = HT.OutPoint
type ChannelMap = DiskMap Key ChanState
