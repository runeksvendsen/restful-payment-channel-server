{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module  PayChanServer.Config.Types where

import           ChanStore.Lib.Types (ConnManager)
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Crypto.Secp256k1 as Secp
import           Control.Lens.TH (makeLenses)
import qualified Data.ByteString as BS
import           Data.Configurator.Types
import           Common.Common (fromHexString)
import           Data.Ratio
import           Data.String.Conversions (cs)



data App = App
 { _dbConn :: ConnManager
 , _pubKey          :: HC.PubKey
 , _openPrice       :: BitcoinAmount
 , _settlePeriod    :: Int
 , _fundingMinConf  :: Int
 , _basePath        :: BS.ByteString
 , _settleChanFunc  :: ReceiverPaymentChannel -> IO HT.TxHash
 }

-- Template Haskell magic
makeLenses ''App

data BitcoinNet = Mainnet | Testnet3

data DBConf       = DBConf       Host Word Int
data ServerDBConf = ServerDBConf String Word
type Host = BS.ByteString


instance Configured BitcoinNet where
    convert (String "live") = return Mainnet
    convert (String "test") = return Testnet3
    convert _ = Nothing

instance Configured HC.Address where
    convert (String text) = HC.base58ToAddr . cs $ text
    convert _ = Nothing

instance Configured BitcoinAmount where
    convert (Number r) =
        if denominator r /= 1 then Nothing
        else Just . fromIntegral $ numerator r
    convert _ = Nothing

instance Configured HC.PrvKey where
    convert (String text) =
        fmap HC.makePrvKey . Secp.secKey . fromHexString . cs $ text
    convert _ = Nothing
