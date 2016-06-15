{-# LANGUAGE  OverloadedStrings #-}

module BlockchainAPI.Impl.ChainSo where

import BlockchainAPI.Types
import qualified Network.Haskoin.Crypto as HC
import           Data.Aeson         (Value(Object, Array), FromJSON, parseJSON, (.:))
import           Data.Aeson.Types   (Parser, parseMaybe, parseEither)
import           Control.Monad      (mzero)
-- import           Control.Monoid ((<>))
import qualified Data.Text as T
import Data.String.Conversions (cs)
import Data.Vector (toList)
import           Network.Wreq       (get, asJSON, responseBody)
import           Control.Lens ((^.))



chainSoAddressInfo :: T.Text -> IO (JsendResult TxInfo)
chainSoAddressInfo addr =
    fmap (^. responseBody) $ asJSON =<<
    get ("https://chain.so/api/v2/get_tx_received/BTCTEST/" ++ cs addr)

toEither :: Show a => JsendResult a -> Either String (Maybe a)
toEither (Success maybeA) = Right maybeA
toEither e = Left $ "chain.so API: " ++ show e

data JsendResult a =
    Success (Maybe a) |
    Fail T.Text |
    Error T.Text
        deriving Show

instance FromJSON a => FromJSON (JsendResult a) where
    parseJSON (Object v) =
        (v .: "status") >>=
        \status -> case (status :: T.Text) of
            "fail" -> Fail <$> v .: "message"
            "error" -> return $ Error "error" -- <$> fmap (cs . show) (v .: "data")
            "success" -> Success . parseMaybe parseJSON <$> (v .: "data" >>= (.: "txs"))
    parseJSON _ = fail "expected object"


instance FromJSON TxInfo where
    parseJSON (Array vec) =
        case toList vec of
            [] -> mzero
            (Object tx1:_) -> TxInfo <$>
                tx1 .: "txid" <*>
                tx1 .: "confirmations" <*>
                (OutInfo "" <$>
                    ((tx1 .: "value") >>= parseStringBTCAmount) <*>
                    (tx1 .: "output_no"))
    parseJSON _ = fail "expected array"


-- https://chain.so/api/v2/get_tx_received/BTCTEST/2MygyE51oGVLxzXenxa6chzgdYBtSGkiy7m
{-
{
  "status" : "success",
  "data" : {
    "network" : "BTCTEST",
    "address" : "2MygyE51oGVLxzXenxa6chzgdYBtSGkiy7m",
    "txs" : [
      {
        "txid" : "3907a263caac18ed47d69a606a60a15e2032d187059eae47fff690f883e20153",
        "output_no" : 0,
        "script_asm" : "OP_HASH160 46af3f2b461424de29fcc91b2d04c1fccd9dfadd OP_EQUAL",
        "script_hex" : "a91446af3f2b461424de29fcc91b2d04c1fccd9dfadd87",
        "value" : "0.00100000",
        "confirmations" : 0,
        "time" : 1466014739
      }
    ]
  }
}
-}