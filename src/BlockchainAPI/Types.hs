module BlockchainAPI.Types where

import           Data.Bitcoin.PaymentChannel.Types (FundingTxInfo(..))
import           Data.Bitcoin.PaymentChannel.Util (parseJSONInt)
import           Network.Haskoin.Transaction  (TxHash)
import qualified Data.Text as T
import           Data.Aeson   (withScientific, withText, Value)
import           Data.Aeson.Types   (Parser)
import           Data.String.Conversions (cs)
import           Data.Scientific (Scientific)


-- TODO: Provide generic interface

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


parseBTCAmount :: Value -> Parser Integer
parseBTCAmount = withScientific "bitcoin amount" $
    parseJSONInt . (* 1e8)

parseStringBTCAmount :: Value -> Parser Integer
parseStringBTCAmount = withText "string bitcoin amount" $
    parseJSONInt . (* 1e8) . read . cs
