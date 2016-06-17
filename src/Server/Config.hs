{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Config where

import           Common.Common (fromHexString)
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)

import qualified Network.Haskoin.Crypto as HC
import qualified Crypto.Secp256k1 as Secp

import           Control.Lens.TH (makeLenses)
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
 }

-- Template Haskell magic
makeLenses ''App


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

-- mkPrivKey :: String -> Maybe HC.PrvKey
-- mkPrivKey = fmap HC.makePrvKey . Secp.secKey . fromHexString

-- pubKeyServer = HC.derivePubKey prvKeyServer
-- prvKeyServer = HC.makePrvKey $ fromJust $ Secp.secKey $ fromHexString
--                        "456af7395529404380edc4fa35a161e096aa510610b98d3d219dc178dc58c1d7"

-- fundsDestAddr :: HC.Address
-- fundsDestAddr = "mmA2XECa7bVERzQKkyy1pNBQ3PC4HnxTC5"

-- settlement tx b86014b07c49425b6011c267e35bb261c07781933f472a6f83541ba0b656d4c8 314 bytes
-- settlementTxFee :: BitcoinAmount
-- settlementTxFee = 15650 -- 50 satoshi/byte


calcSettlementFeeSPB :: BitcoinAmount -> BitcoinAmount
calcSettlementFeeSPB satoshisPerByte = 314 * satoshisPerByte

-- openPrice = settlementTxFee + 1000 :: BitcoinAmount
-- minConf = 0 :: Int
-- settlementPeriodHours = 6 :: Int