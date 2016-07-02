{-# LANGUAGE OverloadedStrings #-}

module BlockchainAPI.Impl.ToshiIo where

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)

import           Data.Aeson.Types   (Parser, parseMaybe, parseEither)
import           Data.Aeson         (Value(Object), FromJSON, parseJSON, (.:))
import           Control.Applicative (liftA2)

-- https://testnet3.toshi.io/api/v0/addresses/2Mt3G9hu7Vy9uKdbDpiEkQYmfHmJVu3ZFic/transactions
-- 404 - {"error":"Not Found"}


data ToshiOut = ToshiOut BitcoinAmount Bool HC.Address

instance FromJSON ToshiOut where
    parseJSON (Object v) =
        ToshiOut <$>
            v .: "amount" <*>
            v .: "spent" <*>
            fmap head (v .: "addresses")


data ToshiTx = ToshiTx HT.TxHash Word [ToshiOut]

instance FromJSON ToshiTx where
    parseJSON (Object v) =
        ToshiTx <$>
            v .: "hash" <*>
            v .: "confirmations" <*>
            v .: "outputs"
            -- liftA2 (++) (v .: "transactions") (v .: "unconfirmed_transactions")

-- "unconfirmed_transactions":[
--     {
--       "hash":"8219c15a18a4e5064e006fec394b4b82f53ecd4e5fc17945f16c4b1b92f01c91",
--       "version":1,
--       "lock_time":892430,
--       "size":223,
--       "inputs":[
--         ...
--       ],
--       "outputs":[
--         {
--           "amount":134500,
--           "spent":false,
--           "script":"OP_HASH160 08b4f07a1e3de9908c8147494d87f757723dc360 OP_EQUAL",
--           "script_hex":"a91408b4f07a1e3de9908c8147494d87f757723dc36087",
--           "script_type":"p2sh",
--           "addresses":[
--             "2Mt3G9hu7Vy9uKdbDpiEkQYmfHmJVu3ZFic"
--           ]
--         },
--         {
--           "amount":4915785,
--           "spent":false,
--           "script":"OP_DUP OP_HASH160 7d0e69b5332247ed16c1ae4ddacd86e3e50a01a3 OP_EQUALVERIFY OP_CHECKSIG",
--           "script_hex":"76a9147d0e69b5332247ed16c1ae4ddacd86e3e50a01a388ac",
--           "script_type":"hash160",
--           "addresses":[
--             "mrvC2TeAaDmEXfTbxLesBNHfN72yXQbLHH"
--           ]
--         }
--       ],
--       "amount":5050285,
--       "fees":51800,
--       "confirmations":0,
--       "pool":"memory"
--     }
--   ],


-- https://bitcoin|testnet3.toshi.io/api/v0/addresses/<address>/transactions

-- {
--  "hash": "12c6DSiU4Rq3P4ZxziKxzrL5LmMBrzjrJX",
--   "balance": 5002467686,
--   "received": 5002477686,
--   "sent": 0,
--   "unconfirmed_received": 10000,
--   "unconfirmed_sent": 0,
--   "unconfirmed_balance": 5002477686,
--   "transactions": [
--     {
--       "hash": "24087a08309ea5796ef139e65f13ce10db1e4465057b665b9d5102a640aac6be",
--       "version": 1,
--       "lock_time": 0,
--       "size": 257,
--       "inputs": [{
--         "previous_transaction_hash": "4aecda969d15b7a75db66b6a90a8cf95f801cc2f68c0699a2816ae41252d9294",
--         "output_index": 1,
--         "amount": 1011640,
--         "script": "3044022017d58d70df1adabee104a8ba1d53d0b520cfed73b4a7e3631a474b7b5423f56e02207cc2be7d6112a63ff678efb9f09b07a1c66983a17a6a7fae85a114b80ca30ed701 04306ae0a0853ac8a40547d243e194146ea0df26b304795d3bfe7879507522120f4fa907593eed843987f91b52632a63b02b5aedbfec744e4fe0bc0b814ae11da1",
--         "addresses": [
--           "15djQ6BzrB766ovRzen3ReVtJzdfzDWwsk"
--         ]
--       }],
--       "outputs": [
--         {
--           "amount": 1000,
--           "spent": false,
--           "script": "OP_DUP OP_HASH160 119b098e2e980a229e139a9ed01a469e518e6f26 OP_EQUALVERIFY OP_CHECKSIG",
--           "script_hex": "76a914119b098e2e980a229e139a9ed01a469e518e6f2688ac",
--           "script_type": "hash160",
--           "addresses": [
--             "12c6DSiU4Rq3P4ZxziKxzrL5LmMBrzjrJX"
--           ]
--         }
--       ],
--       "amount": 0,
--       "fees": 0
--     }
--   ],
--   "unconfirmed_transactions": [
--    {
--       "hash": "7f66c5e6a8bb4b9e640dfcb097232c740a43481dc02817959f48c48d3436b583",
--       "version": 1,
--       "lock_time": 0,
--       "size": 258,
--       "inputs": [],
--       "outputs": [{
--         "amount": 10000,
--         "spent": false,
--         "script": "OP_DUP OP_HASH160 119b098e2e980a229e139a9ed01a469e518e6f26 OP_EQUALVERIFY OP_CHECKSIG",
--         "script_hex": "76a914119b098e2e980a229e139a9ed01a469e518e6f2688ac",
--         "script_type": "hash160",
--         "addresses": [
--           "12c6DSiU4Rq3P4ZxziKxzrL5LmMBrzjrJX"
--         ]
--       }, {
--         "amount": 732000,
--         "spent": false,
--         "script": "OP_DUP OP_HASH160 402319e566a996b9b512cb391352148c15b7a1f2 OP_EQUALVERIFY OP_CHECKSIG",
--         "script_hex": "76a914402319e566a996b9b512cb391352148c15b7a1f288ac",
--         "script_type": "hash160",
--         "addresses": [
--           "16r8J9bmThZCSN2qeKza6btdMk4bb8rnEh"
--         ]
--       }],
--       "amount": 0,
--       "fees": 0,
--       "confirmations": 0,
--       "pool": "orphan"
--     }
--   ]
-- }