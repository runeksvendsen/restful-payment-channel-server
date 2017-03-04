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

module BlockchainAPI.Impl.Bitcoind.Interface
(
    Interface(publishTx, unspentOuts)
  , mkBtcInterface
  , dummyBtcInterface
  , module BlockchainAPI.Impl.Bitcoind.Types
)

where

import           BlockchainAPI.Types

import qualified ConnManager.Servant as Servant
import           ConnManager.Types

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Network.HTTP.Client (Manager)

import qualified APISpec.Blockchain as APISpec
import           BlockchainAPI.Impl.Bitcoind.Types

import           PaymentChannel         (FundingTxInfo(..), BtcAmount, mkNonDusty)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC


data Interface = Interface {
    publishTx   ::  HT.Tx       -> IO (Either String HT.TxHash)
  , unspentOuts ::  HC.Address  -> IO (Either String [AddressFundingInfo])
}

mkBtcInterface :: ConnManager2 -> Interface
mkBtcInterface (Conn2 baseUrl man) =
    Interface
        (\tx    -> Servant.runReq $ publishTx' tx man baseUrl)
        (\addr  -> Servant.runReq $ unspentOutputs' addr man baseUrl)

-- |Used for testing
dummyBtcInterface :: Interface
dummyBtcInterface = Interface (return . Right . HT.txHash) (const $ return . Right $ [])

api :: Proxy APISpec.BlockchainApi
api = Proxy

unspentOutputs' :: HC.Address -> Manager -> BaseUrl -> ClientM [AddressFundingInfo]
publishTx'      :: HT.Tx      -> Manager -> BaseUrl -> ClientM HT.TxHash
(_ :<|> unspentOutputs' :<|> publishTx') = client api
















instance BlockchainAPI Interface where
    listUnspentOutputs (Interface _ listUnspent) addr =
        fmap (map toTxInfo) <$> listUnspent addr
    publishTx (Interface pubTx _) = pubTx

toTxInfo :: AddressFundingInfo -> TxInfo
toTxInfo (AddressFundingInfo _ txid vout numConfs val) =
    TxInfo numConfs $ CFundingTxInfo txid vout $
        -- TODO
        either (error "TODO: Dusty funding amount") id $ mkNonDusty (fromIntegral val :: BtcAmount)
