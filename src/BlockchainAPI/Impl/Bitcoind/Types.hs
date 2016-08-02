{-# LANGUAGE OverloadedStrings #-}

module BlockchainAPI.Impl.Bitcoind.Types where

import           BlockchainAPI.Types (parseBitcoinAmount)
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)
import           Data.Aeson         (Value(Object, Array), FromJSON(..), (.:))
import           Control.Monad (mzero)
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import           Data.Word (Word32)

-- | Holds information about an output paying to an address
data AddressFundingInfo = AddressFundingInfo {
    asiDestAddress  ::  HC.Address
   ,asiFundingTxId  ::  HT.TxHash
   ,asiFundingVout  ::  Word32
   ,asiConfs        ::  Integer
   ,asiValue        ::  BitcoinAmount
} deriving (Eq, Show)

instance FromJSON AddressFundingInfo where
    parseJSON (Object v) =
        AddressFundingInfo <$>
            v .: "address" <*>
            v .: "funding_txid" <*>
            v .: "funding_vout" <*>
            v .: "confirmations" <*>
            (v .: "value" >>= parseBitcoinAmount)
    parseJSON _ = mzero

