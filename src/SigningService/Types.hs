{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SigningService.Types where

import           Server.Types
import           Snap (Snap)
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount, PayChanError)
import qualified Network.Haskoin.Transaction as HT
import           Control.Lens.TH (makeLenses)

type SettlementTxId = HT.TxHash

data AppConf = AppConf
 { _makeSettlementTxFunc    :: (ReceiverPaymentChannel, BitcoinAmount) -> Either PayChanError HT.Tx
 }

-- Template Haskell magic
makeLenses ''AppConf
