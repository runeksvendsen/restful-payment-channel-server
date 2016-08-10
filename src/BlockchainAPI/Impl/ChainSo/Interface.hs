module BlockchainAPI.Impl.ChainSo.Interface where

import           BlockchainAPI.Impl.ChainSo.ChainSo
import           BlockchainAPI.Types

data Interface = Interface

instance BlockchainAPI Interface where
    listUnspentOutputs Interface = addressInfoEither
    publishTx _ = error "Not implemented"
