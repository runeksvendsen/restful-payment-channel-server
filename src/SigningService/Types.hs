{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SigningService.Types where

import           PayChanServer.Types
import           Snap (Snap)
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount,
                                                    PayChanError, RecvPubKey)
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import           Control.Lens.TH (makeLenses)

type SettlementTxId = HT.TxHash

data AppConf = AppConf
 {  _pubKey                 :: RecvPubKey
 ,  _makeSettlementTxFunc   :: (ReceiverPaymentChannel, BitcoinAmount) -> HT.Tx
 }

-- Template Haskell magic
makeLenses ''AppConf


data SigningSettleConfig = SigningSettleConfig {
    confSettlePrivKey     :: HC.PrvKey,
    confSettleRecvAddr    :: HC.Address
}

