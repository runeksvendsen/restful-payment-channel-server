{-# LANGUAGE  OverloadedStrings, FlexibleInstances #-}

module BlockchainAPI.Impl.ChainSo.ChainSo where

import           BlockchainAPI.Types
import qualified Network.Haskoin.Constants as HCC
import qualified Network.Haskoin.Crypto as HC
import           Data.Aeson         (Value(Object, Array), FromJSON, parseJSON, (.:), encode)
import           Control.Monad      (mzero)
import qualified Data.Text as T
import           Data.String.Conversions (cs)
import           Network.Wreq       (get, asJSON, responseBody)
import           Control.Lens ((^.))
import           Data.Bitcoin.PaymentChannel.Types (FundingTxInfo(..))


addressInfoEither :: HC.Address -> IO (Either String [TxInfo])
addressInfoEither = fmap toEither . chainSoAddressInfo

chainSoAddressInfo :: HC.Address -> IO (JsendResult [TxInfo])
chainSoAddressInfo addr =
    fmap (^. responseBody) $ asJSON =<<
    get ("https://chain.so/api/v2/get_tx_unspent/" ++ netStr ++ "/" ++ cs (encode addr) )
        where netStr = case HCC.getNetworkName HCC.getNetwork of
                "prodnet" -> "BTC"
                "testnet" -> "BTCTEST"
                _         -> error "BUG: unknown Bitcoin network"


toEither :: Show a => JsendResult a -> Either String a
toEither (Success a) = Right a
toEither e = Left $ "chain.so API: " ++ show e

data JsendResult a =
    Success a |
    Fail T.Text |
    Error T.Text
        deriving Show

instance FromJSON (JsendResult [TxInfo]) where
    parseJSON (Object v) =
        (v .: "status") >>=
        \status -> case (status :: T.Text) of
            "fail" -> Fail <$> v .: "message"
            "error" -> return $ Error "error" -- <$> fmap (cs . show) (v .: "data")
            "success" -> Success <$> (v .: "data" >>= (.: "txs"))
            _         -> fail "jsend parse fail: expected either: 'fail', 'error' or 'success'"
    parseJSON _ = fail "expected object"


instance FromJSON TxInfo where
    parseJSON = parseTxInfo

parseTxInfo (Object o) = TxInfo <$>
    o .: "confirmations" <*>
    (CFundingTxInfo <$>
        o .: "txid" <*>
        o .: "output_no" <*>
        o .: "value")
parseTxInfo _ = mzero
