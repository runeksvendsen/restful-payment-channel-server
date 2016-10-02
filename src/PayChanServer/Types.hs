{-# LANGUAGE DeriveGeneric #-}
module PayChanServer.Types
(
    module PayChanServer.Types
  , module Common.Types
)
 where

import           PayChanServer.Config.Types
import           Common.Types
import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics

-- |The monad in which PayChanServer's API handlers run
type AppPC = AppM App

data ChannelInfo = ChannelInfo
  { sender          :: SendPubKey
  , status          :: ChannelStatus
  , value_left      :: BitcoinAmount
  , expires         :: BitcoinLockTime
  , funding_value   :: BitcoinAmount
  , funding_address :: Address
  , funding_source  :: OutPoint
  , settlement_txid :: Maybe TxHash
  } deriving Generic

instance ToJSON ChannelInfo
instance FromJSON ChannelInfo
