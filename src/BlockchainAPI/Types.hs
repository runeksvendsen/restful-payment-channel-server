module BlockchainAPI.Types where

import           Data.Bitcoin.PaymentChannel.Types (FundingTxInfo(..), BitcoinAmount)
import           Data.Bitcoin.PaymentChannel.Util (parseJSONInt)
import           Data.Aeson   (withScientific, withText, Value)
import           Data.Aeson.Types   (Parser)
import           Data.String.Conversions (cs)
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT


-- |Types which can be used to query the Bitcoin blockchain
class BlockchainAPI a where
    listUnspentOutputs  :: a -> HC.Address  -> IO (Either String [TxInfo])
    publishTx           :: a -> HT.Tx       -> IO (Either String HT.TxHash)

toFundingTxInfo :: TxInfo -> FundingTxInfo
toFundingTxInfo (TxInfo _ fi) = fi

data TxInfo = TxInfo {
    txConfs       ::  Integer  -- ^Number of confirmations
  , fundingInfo   ::  FundingTxInfo
} deriving Show

parseBitcoinAmount :: Value -> Parser BitcoinAmount
parseBitcoinAmount val = fmap fromIntegral (parseBTCAmount val)

parseBTCAmount :: Value -> Parser Integer
parseBTCAmount = withScientific "bitcoin amount" $
    parseJSONInt . (* 1e8)

parseStringBTCAmount :: Value -> Parser Integer
parseStringBTCAmount = withText "string bitcoin amount" $
    parseJSONInt . (* 1e8) . read . cs
