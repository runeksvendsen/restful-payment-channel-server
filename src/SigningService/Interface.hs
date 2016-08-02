{-|
Module      : SigningService.Interface
Description : Publish settlement transactions when given payment channel states
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

A service that runs somewhere safe, although reachable from the outside, which keeps the
 server private key, and produces a settlement transaction for each ReceiverPaymentChannel
 it receives. This service is configured with the private key whose public
 counterpart is advertized by PayChanServer, as well as a funds destination address,
 and will only produce settlement transactions paying to this pre-defined address.

This file describes the server interface (currently comprising 'signSettlementTx' only).
-}


module SigningService.Interface
(
    signSettlementTx
,   getPubKey
)
where

import           SigningService.Spec
-- import           ConnManager.Types ()
import           ConnManager.RequestRunner (ConnManager, runRequest)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount, RecvPubKey)
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC

getPubKey :: ConnManager -> IO RecvPubKey
getPubKey conn = runRequest conn GetPubKey

-- |Produce settlement transaction(s) by POSTing a list of states to the endpoint /settle_channel
signSettlementTx :: ConnManager -> BitcoinAmount -> ReceiverPaymentChannel -> IO HT.Tx
signSettlementTx conn txFee rpc = runRequest conn $ SettleChan rpc txFee
