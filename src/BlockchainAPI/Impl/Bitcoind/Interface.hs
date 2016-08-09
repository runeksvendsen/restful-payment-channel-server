{-|
https://github.com/runeksvendsen/blockchain-restful-address-index/

Implementation of client API for above RESTful server which interfaces with
addr-index patched bitcoind[1] (regular bitcoind has no address-to-transaction-id index).

Caveat: does not work for unconfirmed transactions. The address index doesn't index transactions
in the mempool (before they get into the blockchain), so the API will return 404 until
a single confirmation has been obtained.

[1] https://github.com/btcdrak/bitcoin/tree/addrindex-0.12
-}

{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}

module BlockchainAPI.Impl.Bitcoind.Interface where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

-- import           BlockchainAPI.Impl.Bitcoind.Spec
import           BlockchainAPI.Impl.Bitcoind.Types
-- import           ConnManager.RequestRunner (ConnManager, runRequestJSON)

import           APISpec.Blockchain (api)

import qualified Network.Haskoin.Crypto as HC

testAddrTestnet = "2N414xMNQaiaHCT5D7JamPz7hJEc9RG7469"

unspentOutputs :<|> publishTx = client api

test1 = unspentOutputs

test2 = publishTx

t = unspentOutputs testAddrTestnet (BaseUrl Http "localhost" 8000 "")

getUnspent :: ConnManager -> HC.Address -> IO [AddressFundingInfo]
getUnspent conn addr = runRequestJSON conn $ GetUnspent addr


data BtcInterface = BtcInteface {
    publishTx   ::  HT.Tx       -> IO (Either String HT.TxHash)
  , unspentOuts ::  HC.Address  -> IO
}