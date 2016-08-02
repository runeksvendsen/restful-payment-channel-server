{-|
https://github.com/runeksvendsen/blockchain-restful-address-index/

Implementation of client API for above RESTful server which interfaces with
addr-index patched bitcoind[1] (regular bitcoind has no address-to-transaction-id index).

Caveat: does not work for unconfirmed transactions. The address index doesn't index transactions
in the mempool (before they get into the blockchain), so the API will return 404 until
a single confirmation has been obtained.

[1] https://github.com/btcdrak/bitcoin/tree/addrindex-0.12
-}

module BlockchainAPI.Impl.Bitcoind.Interface where

import           BlockchainAPI.Impl.Bitcoind.Spec
import           BlockchainAPI.Impl.Bitcoind.Types
import           ConnManager.RequestRunner (ConnManager, runRequestJSON)

import qualified Network.Haskoin.Crypto as HC


getUnspent :: ConnManager -> HC.Address -> IO [AddressFundingInfo]
getUnspent conn addr = runRequestJSON conn $ GetUnspent addr

