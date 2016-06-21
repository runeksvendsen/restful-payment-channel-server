{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Config where

import           Common.Common (fromHexString)
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Constants as HCC
import qualified Crypto.Secp256k1 as Secp

import           Control.Lens.TH (makeLenses)
import qualified Data.ByteString as BS
import           Data.Ratio
import           Data.Maybe (fromJust)
import           Data.Configurator.Types
import           Data.String.Conversions (cs)
import           Server.ChanStore (ChannelMap)
import           Server.Types


data App = App
 { _channelStateMap :: ChannelMap
 , _settleConfig    :: ChanSettleConfig
 , _pubKey          :: HC.PubKey
 , _openPrice       :: BitcoinAmount
 , _fundingMinConf  :: Int
 , _basePath        :: BS.ByteString
 , _hostname        :: String
 }

-- Template Haskell magic
makeLenses ''App

data BitcoinNet = Mainnet | Testnet3

setBitcoinNetwork :: BitcoinNet -> IO ()
setBitcoinNetwork Mainnet = return ()
setBitcoinNetwork Testnet3 = HCC.switchToTestnet3

toPathString :: BitcoinNet -> BS.ByteString
toPathString Mainnet = "live"
toPathString Testnet3 = "test"

instance Configured HC.Address where
    convert (String text) = HC.base58ToAddr . cs $ text

instance Configured BitcoinAmount where
    convert (Number r) =
        if denominator r /= 1 then Nothing
        else Just . fromIntegral $ numerator r
    convert _ = Nothing

instance Configured HC.PrvKey where
    convert (String text) =
        fmap HC.makePrvKey . Secp.secKey . fromHexString . cs $ text

instance Configured BitcoinNet where
    convert (String "live") = return Mainnet
    convert (String "test") = return Testnet3
    convert (String _) = Nothing


calcSettlementFeeSPB :: BitcoinAmount -> BitcoinAmount
calcSettlementFeeSPB satoshisPerByte = 314 * satoshisPerByte -- 346 2 outputs

-- openPrice = settlementTxFee + 1000 :: BitcoinAmount
-- minConf = 0 :: Int
-- settlementPeriodHours = 6 :: Int