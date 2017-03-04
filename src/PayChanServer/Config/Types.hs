{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, LambdaCase, TypeOperators #-}

module  PayChanServer.Config.Types
(
   module PayChanServer.Config.Types
 , Config
)

where

import           AppPrelude.Types.Orphans ()
import           AppPrelude.Util
--import qualified ChanStore.Interface as Store
import qualified PayChanServer.Callback.Interface as Callback
import           PaymentChannel.Types (ServerPayChanX, BtcAmount)
import qualified PaymentChannel as BTC

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Crypto.Secp256k1 as Secp
import           Control.Lens.TH (makeLenses)
import qualified Data.ByteString as BS
import           Data.Configurator.Types (Config, Configured, convert, Value(..))

import           Data.Ratio

import qualified BlockchainAPI.Types as BtcType
import qualified Data.Tagged as Tag



-- import           Servant.Server.Internal.Context (Context(EmptyContext, (:.)))

--- Tagged types
getVal = Tag.unTagged

data BTCConf   = BTCConf
data SettleHrs = SettleHrs
data Charge    = Charge
data Dust      = Dust
data ChanDur   = ChanDur
data Callback  = Callback

type BtcConf      = Tag.Tagged BTCConf Word
type SettleHours  = Tag.Tagged SettleHrs Word
type OpenPrice    = Tag.Tagged Charge BtcAmount
type DustLimit    = Tag.Tagged Dust BtcAmount
type DurationHours= Tag.Tagged ChanDur Word

data ChanConf = ChanConf
  { btcMinConf      :: BtcConf
  , openPrice       :: OpenPrice
  , dustLimit       :: DustLimit
  , settlePeriod'   :: SettleHours
  , minDuration     :: DurationHours
  }

--toBtcConf :: ChanConf -> BTCConf.Config
--toBtcConf ChanConf{..} = BTCConf.Config
--    { cDustLimit = BTC.getDustLimit
--    , cSettlementPeriod = Tag.Tagged . fromIntegral $ Tag.unTagged settlePeriod'
--    }

data App = App
 { _callbackIface   :: Callback.Interface
 , _listUnspent     :: HC.Address  -> IO (Either String [BtcType.TxInfo])
 , _settleChannel   :: ServerPayChanX -> IO HT.TxHash    -- Dummy function if debug is enabled
 , _chanConf        :: ChanConf
 , _settlePeriod    :: Word
 , _basePath        :: BS.ByteString
 , _areWeDebugging  :: Bool
 }

-- Template Haskell magic
makeLenses ''App

data BitcoinNet = Mainnet | Testnet3

data DBConf       = DBConf       Host Word Int
data ServerDBConf = ServerDBConf String Word
type Host = BS.ByteString

getWord r = if denominator r /= 1
    then Nothing
    else Just $ numerator r

instance Configured BitcoinNet where
    convert (String "live") = return Mainnet
    convert (String "test") = return Testnet3
    convert _ = Nothing

instance Configured HC.Address where
    convert (String text) = HC.base58ToAddr . cs $ text
    convert _ = Nothing

instance Configured BtcAmount where
    convert (Number r) = fmap fromIntegral (getWord r)
    convert _ = Nothing

-- Decode private key as 64 hex chars
instance Configured HC.PrvKey where
    convert (String text) =
        either (const Nothing) (fmap HC.makePrvKey . Secp.secKey)
            (hexDecode (cs text :: BS.ByteString))
    convert _ = Nothing

instance Configured BtcConf where
    convert (Number r) = fmap (Tag.Tagged . fromIntegral) (getWord r)
    convert _ = Nothing

instance Configured SettleHours where
    convert (Number r) = fmap (Tag.Tagged . fromIntegral) (getWord r)
    convert _ = Nothing

instance Configured DustLimit where
    convert (Number r) = fmap (Tag.Tagged . fromIntegral) (getWord r)
    convert _ = Nothing

instance Configured OpenPrice where
    convert (Number r) = fmap (Tag.Tagged . fromIntegral) (getWord r)
    convert _ = Nothing

instance Configured DurationHours where
    convert (Number r) = fmap (Tag.Tagged . fromIntegral) (getWord r)
    convert _ = Nothing

data ServerSettleConfig = ServerSettleConfig {
    confSettleTxFee       :: BtcAmount,
    confSettlePeriod      :: SettleHours
}
