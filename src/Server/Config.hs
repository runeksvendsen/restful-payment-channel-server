{-# LANGUAGE  OverloadedStrings #-}

module Server.Config where

import           Common.Common (fromHexString)
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)

import qualified Network.Haskoin.Crypto as HC
import qualified Crypto.Secp256k1 as Secp

import           Data.Maybe (fromJust)


pubKeyServer = HC.derivePubKey prvKeyServer
prvKeyServer = HC.makePrvKey $ fromJust $ Secp.secKey $ fromHexString
                       "456af7395529404380edc4fa35a161e096aa510610b98d3d219dc178dc58c1d7"

fundsDestAddr :: HC.Address
fundsDestAddr = "mmA2XECa7bVERzQKkyy1pNBQ3PC4HnxTC5"

-- settlement tx b86014b07c49425b6011c267e35bb261c07781933f472a6f83541ba0b656d4c8 313 bytes
settlementTxFee :: BitcoinAmount
settlementTxFee = 15650 -- 50 satoshi/byte


openPrice = settlementTxFee + 1000 :: BitcoinAmount
minConf = 0 :: Int
minChannelDuration = 12 * 3600 :: Integer
settlementPeriodHours = 6 :: Int