{-# LANGUAGE DeriveGeneric #-}
module PayChanServer.Types
( module PayChanServer.Types
, module AppPrelude.Types
, module PaymentChannel.Types
)
 where

import           PayChanServer.Config.Types
import           AppPrelude.Types
import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics
import           PaymentChannel.Types hiding (Get)
import qualified RBPCP.Types as RBPCP
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC


class PayChanConf m where
    chanConf :: m App

class PayChanDB m db where
    dbConf :: m db


-- |The monad in which PayChanServer's API handlers run
type AppPC db = AppM App

data ChannelInfo = ChannelInfo
  { sender          :: SendPubKey
  , status          :: RBPCP.ChannelStatus
  , value_left      :: BtcAmount
  , expires         :: LockTimeDate
  , funding_value   :: BtcAmount
  , funding_address :: HC.Address
  , funding_source  :: HT.OutPoint
  , settlement_txid :: Maybe HT.TxHash
  } deriving Generic

instance ToJSON ChannelInfo
instance FromJSON ChannelInfo
