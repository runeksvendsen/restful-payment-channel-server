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

settlementTxFee :: BitcoinAmount
settlementTxFee = 100000

