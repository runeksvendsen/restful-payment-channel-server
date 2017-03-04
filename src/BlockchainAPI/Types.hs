module BlockchainAPI.Types where

import           Data.Word (Word64)
import           PaymentChannel.Types (FundingTxInfo(..), BtcAmount)
import           PaymentChannel.Util (parseJSONWord)
import           Data.Aeson   (withScientific, withText, Value)
import           Data.Aeson.Types   (Parser)
import           Data.String.Conversions (cs)
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT


-- |Types which can be used to query the Bitcoin blockchain (not used yet)
class BlockchainAPI a where
    listUnspentOutputs  :: a -> HC.Address  -> IO (Either String [TxInfo])
    publishTx           :: a -> HT.Tx       -> IO (Either String HT.TxHash)

toFundingTxInfo :: TxInfo -> FundingTxInfo
toFundingTxInfo (TxInfo _ fi) = fi

data TxInfo = TxInfo {
    txConfs       ::  Integer  -- ^Number of confirmations
  , fundingInfo   ::  FundingTxInfo
} deriving Show

parseBitcoinAmount :: Value -> Parser BtcAmount
parseBitcoinAmount val = fmap fromIntegral (parseBTCAmount val)

parseBTCAmount :: Value -> Parser Word64
parseBTCAmount = withScientific "bitcoin amount" $
    parseJSONWord . (* 1e8)

parseStringBTCAmount :: Value -> Parser Word64
parseStringBTCAmount = withText "string bitcoin amount" $
    parseJSONWord . (* 1e8) . read . cs
