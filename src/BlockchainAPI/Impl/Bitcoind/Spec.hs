{-# LANGUAGE OverloadedStrings #-}
module BlockchainAPI.Impl.Bitcoind.Spec where

import           BlockchainAPI.Impl.Bitcoind.Types
import           ConnManager.RequestRunner
import           Common.URLParam (pathParamEncode)

import qualified Network.Haskoin.Crypto as HC
import           Data.Monoid ((<>))
import           Data.String.Conversions (cs)
import           Network.Wreq       (get, asJSON, responseBody)
import           Control.Lens ((^.))

data GetUnspent = GetUnspent HC.Address

instance HasReqParams GetUnspent where
    rPath (GetUnspent addr) = "/unspentOutputs/" <> pathParamEncode addr
    rMethod                 = const "GET"
