module BlockchainAPI.Types where

import           Data.Bitcoin.PaymentChannel.Types (FundingTxInfo(..))
import           Network.Haskoin.Transaction  (TxHash)
import qualified Data.Text as T

toFundingTxInfo :: TxInfo -> FundingTxInfo
toFundingTxInfo (TxInfo txId _ (OutInfo _ chanVal idx)) =
    CFundingTxInfo txId (fromIntegral idx) (fromIntegral chanVal)

data OutInfo = OutInfo {
    outAddress  ::  T.Text,
    outAmount   ::  Integer,
    outIndex    ::  Integer
} deriving Show

data TxInfo = TxInfo {
    txId        ::  TxHash,
    txConfs     ::  Integer,
    txOutInfo   ::  OutInfo
} deriving Show
