{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SettlementService.Types where

import           Server.Types
import           Snap (Snap)
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel)
import qualified Network.Haskoin.Transaction as HT
import           Control.Lens.TH (makeLenses)

type SettlementTxId = HT.TxHash

data AppConf = AppConf
 { _settleChannelFunc    :: ReceiverPaymentChannel -> IO (Either String SettlementTxId)
 }

-- Template Haskell magic
makeLenses ''AppConf
