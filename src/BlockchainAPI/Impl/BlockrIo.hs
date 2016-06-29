{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}

module BlockchainAPI.Impl.BlockrIo where

import BlockchainAPI.Types (OutInfo(..), TxInfo(..), parseBTCAmount)
import Common.Common (parseJSONInt)

import           Control.Concurrent (threadDelay)
import           Control.Monad      (mzero, unless)
import           Control.Lens       ((^.))

import           Network.Wreq       (get, asJSON, responseBody)
import           Data.Aeson.Types   (Parser, parseMaybe, parseEither)
import           Data.Aeson         (Value(Object), FromJSON, parseJSON, (.:))
import           Data.Maybe         (listToMaybe, isJust)
import           Network.Haskoin.Transaction  (TxHash, txHashToHex)

import           Text.Printf        (printf)
import qualified Data.Text as T
import           Data.String.Conversions (cs)


instance FromJSON OutInfo where
    parseJSON (Object v) =
        OutInfo <$>
            v .: "address" <*>
            ((v .: "amount") >>= parseBTCAmount) <*>
            ((v .: "n") >>= parseJSONInt)
    parseJSON _ = mzero

waitGetFundingInfo :: String -> Integer -> IO TxInfo
waitGetFundingInfo fundAddr numConfs =
    waitGetFundingTxId fundAddr numConfs >>=
    waitGetConfirmedTxInfo fundAddr numConfs

waitGetFundingTxId :: String -> Integer -> IO TxHash
waitGetFundingTxId fundAddr numConfs = do
    maybeTxId <- txIDFromAddr fundAddr
    case maybeTxId of
            Nothing ->
                putStrLn "No funding received." >>
                threadDelay (round $ 20 * 1e6) >>
                waitGetFundingTxId fundAddr numConfs
            Just txId -> return txId

waitGetConfirmedTxInfo :: String -> Integer -> TxHash -> IO TxInfo
waitGetConfirmedTxInfo fundAddr txId numConfs = do
    maybeTxInf <- getConfirmedTxInfo fundAddr numConfs txId
    case maybeTxInf of
        Nothing -> do
            threadDelay (round $ 20 * 1e6)
            waitGetConfirmedTxInfo fundAddr txId numConfs
        Just txInf -> return txInf

getConfirmedTxInfo :: String -> TxHash -> Integer -> IO (Maybe TxInfo)
getConfirmedTxInfo fundAddr txId numConfs = do
    txInfo <- getFundOutOrFail fundAddr txId
    if (txConfs txInfo) >= numConfs then
        return (Just txInfo)
    else
        putStrLn (printf "Confirmations: %d/%d." (txConfs txInfo) numConfs) >> return Nothing

tryGetFundingInfo :: String -> IO (Maybe TxInfo)
tryGetFundingInfo fundAddr = do
    maybeTxId <- txIDFromAddr fundAddr
    maybe (return Nothing) (fmap Just . getFundOutOrFail fundAddr) maybeTxId



getFundOutOrFail :: String -> TxHash -> IO TxInfo
getFundOutOrFail fundAddr txId = do
    fundOut <- fundingOutInfoFromTxId fundAddr txId
    either error return fundOut

fundingOutInfoFromTxId :: String -> TxHash -> IO (Either String TxInfo)
fundingOutInfoFromTxId fundAddr txId =
    fmap (parseEither (parseTxInfo fundAddr)) (getTxJSON txId)

---get funding tx id---
-- | Return the transaction ID of the first transaction paying to fundAddr
txIDFromAddr :: String -> IO (Maybe TxHash)
txIDFromAddr fundAddr = fmap (parseMaybe parseTxID) (getAddrJSON fundAddr)

getAddrJSON :: String -> IO Value
getAddrJSON fundingAddr =
    fmap (^. responseBody) $ asJSON =<<
    get ("http://tbtc.blockr.io/api/v1/address/info/" ++ fundingAddr)

parseTxID :: Value -> Parser TxHash
parseTxID (Object v) =
    v .: "data" >>= (.: "first_tx") >>= (.: "tx")
parseTxID v = fail $ "expected JSON Object from address query, got: " ++ show v
---(get funding tx id)---

---get funding tx output---
getTxJSON :: TxHash -> IO Value
getTxJSON txId =
    fmap (^. responseBody) $ asJSON =<<
    get ("http://tbtc.blockr.io/api/v1/tx/info/" ++ hashToStr txId)
        where hashToStr = cs . txHashToHex


parseFundingOutput :: String -> Value -> Parser OutInfo
parseFundingOutput fundAddr txJSON =
    parseFundingTxOuts txJSON >>= getFundingOutput fundAddr

parseTxInfo :: String -> Value -> Parser TxInfo
parseTxInfo fundAddr txJSON@(Object v) =
    TxInfo <$> (v .: "data" >>= (.: "tx")) <*>
        (v .: "data" >>= (.: "confirmations")) <*>
        parseFundingOutput fundAddr txJSON
parseTxInfo _ v =
    fail $ "expected JSON Object from tx query, got: " ++ show v

parseFundingTxOuts :: Value -> Parser [OutInfo]
parseFundingTxOuts (Object v) =
    v .: "data" >>= (.: "trade") >>=
    (.: "vouts")
parseFundingTxOuts v = fail $ "expected JSON Object from tx query, got: " ++ show v

getFundingOutput :: String -> [OutInfo] -> Parser OutInfo
getFundingOutput fundAddr oiList =
    maybe
        (fail "no outputs in supposed funding transaction match funding address")
        return
        (listToMaybe $
            filter (\(OutInfo oiAddr _ _) -> oiAddr == T.pack fundAddr) oiList)

---(get funding tx output)---



-- curl https://tbtc.blockr.io/api/v1/address/info/2NCTirSGjFM8T7hUow3AcfyFaw1N1APnYuP?confirmations=10 | jq
-- {
--   "status": "success",
--   "data": {
--     "address": "2NCTirSGjFM8T7hUow3AcfyFaw1N1APnYuP",
--     "is_unknown": false,
--     "balance": 0.01,
--     "balance_multisig": 0,
--     "totalreceived": 0.01,
--     "nb_txs": 1,
--     "first_tx": { -- is null if no tx pays to address
--       "time_utc": "2016-05-14T16:10:31Z",
--       "tx": "411aeb3bba69e99065f7d763642e8e8c2607d4839af4520a4c73d3fdf8008222",
--       "block_nb": "846846",
--       "value": 0.01,
--       "confirmations": 189
--     },
--     "last_tx": {
--       "time_utc": "2016-05-14T16:10:31Z",
--       "tx": "411aeb3bba69e99065f7d763642e8e8c2607d4839af4520a4c73d3fdf8008222",
--       "block_nb": "846846",
--       "value": 0.01,
--       "confirmations": 189
--     },
--     "is_valid": true,
--     "confirmations": 10
--   },
--   "code": 200,
--   "message": ""
-- }

-- curl https://tbtc.blockr.io/api/v1/tx/info/411aeb3bba69e99065f7d763642e8e8c2607d4839af4520a4c73d3fdf8008222 | jq
-- {
--   "status": "success",
--   "data": {
--     "tx": "411aeb3bba69e99065f7d763642e8e8c2607d4839af4520a4c73d3fdf8008222",
--     "block": 846846,
--     "confirmations": 189,
--     "time_utc": "2016-05-14T16:10:31Z",
--     "is_coinbased": 0,
--     "trade": {
--       "vins": [
--         {
--           "address": "mtgkTeRjds8r9RAcrHkGa8U4AXMcrejZQr",
--           "is_nonstandard": false,
--           "amount": -0.038406,
--           "n": 0,
--           "type": 0,
--           "vout_tx": "1d7274ba13c285b11ed1fdaa8071409c3450f48447cb4c99ff01fcd238faa12a"
--         }
--       ],
--       "vouts": [
--         {
--           "address": "2NCTirSGjFM8T7hUow3AcfyFaw1N1APnYuP",
--           "is_nonstandard": false,
--           "amount": 0.01,
--           "n": 0,
--           "type": 1,
--           "is_spent": 0
--         },
--         {
--           "address": "n3XfJ41rTzfQPmqiP5aYUS4obBKoU9YkvY",
--           "is_nonstandard": false,
--           "amount": 0.028182,
--           "n": 1,
--           "type": 1,
--           "is_spent": 0
--         }
--       ]
--     },
--     "vins": [
--       {
--         "address": "mtgkTeRjds8r9RAcrHkGa8U4AXMcrejZQr",
--         "is_nonstandard": false,
--         "amount": "-0.03840600",
--         "n": 0,
--         "type": 0,
--         "vout_tx": "1d7274ba13c285b11ed1fdaa8071409c3450f48447cb4c99ff01fcd238faa12a"
--       }
--     ],
--     "vouts": [
--       {
--         "address": "2NCTirSGjFM8T7hUow3AcfyFaw1N1APnYuP",
--         "is_nonstandard": false,
--         "amount": "0.01000000",
--         "n": 0,
--         "type": 1,
--         "is_spent": 0,
--         "extras": {
--           "asm": "OP_HASH160 d2c779fa0134da51237bdd078bd0f8e2ce998837 OP_EQUAL",
--           "script": "a914d2c779fa0134da51237bdd078bd0f8e2ce99883787",
--           "reqSigs": 1,
--           "type": "scripthash"
--         }
--       },
--       {
--         "address": "n3XfJ41rTzfQPmqiP5aYUS4obBKoU9YkvY",
--         "is_nonstandard": false,
--         "amount": "0.02818200",
--         "n": 1,
--         "type": 1,
--         "is_spent": 0,
--         "extras": {
--           "asm": "OP_DUP OP_HASH160 f174fe077f23ad15bf68ebbb42916cba7d0d9398 OP_EQUALVERIFY OP_CHECKSIG",
--           "script": "76a914f174fe077f23ad15bf68ebbb42916cba7d0d939888ac",
--           "reqSigs": 1,
--           "type": "pubkeyhash"
--         }
--       }
--     ],
--     "fee": "0.00022400",
--     "days_destroyed": "0.00",
--     "is_unconfirmed": false,
--     "extras": null
--   },
--   "code": 200,
--   "message": ""
-- }