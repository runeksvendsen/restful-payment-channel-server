{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.Types (
      module Data.Bitcoin.PaymentChannel.Types
    , module Common.Handler.Types
    , module RBPCP.Types
    , ChannelResource(..)
    , MonadIO
    , HT.TxHash
    , HT.OutPoint(..)
    , HC.Signature
    , HS.Script
    , HC.Address
    , CreateResult(..)
    , BS.ByteString
    , JSONString
    , T.Text
) where

import           Data.Bitcoin.PaymentChannel.Types
import           RBPCP.Types hiding (Payment)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto      as HC
import qualified Network.Haskoin.Script      as HS

import qualified Data.ByteString             as BS
import qualified Data.Serialize              as Bin
import qualified Data.Text                   as T

import           Common.Handler.Types
import           Control.Monad.IO.Class (MonadIO)
import           Data.DiskMap (CreateResult(..))
import           GHC.Generics

data ChannelResource = ChannelResource SendPubKey BitcoinLockTime HT.OutPoint
        deriving Generic

instance Bin.Serialize ChannelResource
type JSONString = T.Text