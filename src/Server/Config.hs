{-# LANGUAGE OverloadedStrings #-}

module Server.Config
(
loadConfig,configLookupOrFail,getSettleConfig,getBitcoindConf,
BitcoinNet,
setBitcoinNetwork,toPathString,

-- re-exports
Config

)
where

import           Server.Config.Types
import           Common.Common (fromHexString)
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Constants as HCC
import qualified Crypto.Secp256k1 as Secp

import           Control.Lens.TH (makeLenses)
import qualified Data.ByteString as BS
import           Data.Ratio
import           Data.Configurator.Types
import qualified Data.Configurator as Conf
import           Data.String.Conversions (cs)
import           Server.ChanStore.Client (ChanMapConn)
import           Server.Types
import           Bitcoind (BTCRPCInfo(..))



loadConfig :: String -> IO Config
loadConfig confFile = Conf.load [Conf.Required confFile]

configLookupOrFail :: Configured a => Config -> Name -> IO a
configLookupOrFail conf name =
    Conf.lookup conf name >>= maybe
        (fail $ "ERROR: Failed to read key \"" ++ cs name ++
            "\" in config (key not present or invalid)")
        return

getSettleConfig :: Config -> IO ChanSettleConfig
getSettleConfig cfg = SettleConfig <$>
        configLookupOrFail cfg "settlement.privKeySeed" <*>
        configLookupOrFail cfg "settlement.fundsDestinationAddress" <*>
        fmap calcSettlementFeeSPB (configLookupOrFail cfg "settlement.txFeeSatoshiPerByte") <*>
        configLookupOrFail cfg "settlement.settlementPeriodHours"

getBitcoindConf :: Config -> IO BTCRPCInfo
getBitcoindConf cfg = BTCRPCInfo <$>
    configLookupOrFail cfg "bitcoin.bitcoindRPC.ip" <*>
    configLookupOrFail cfg "bitcoin.bitcoindRPC.port" <*>
    configLookupOrFail cfg "bitcoin.bitcoindRPC.user" <*>
    configLookupOrFail cfg "bitcoin.bitcoindRPC.pass"

calcSettlementFeeSPB :: BitcoinAmount -> BitcoinAmount
calcSettlementFeeSPB satoshisPerByte = 331 * satoshisPerByte -- 346 2 outputs

setBitcoinNetwork :: BitcoinNet -> IO ()
setBitcoinNetwork Mainnet = return ()
setBitcoinNetwork Testnet3 = HCC.switchToTestnet3

toPathString :: BitcoinNet -> BS.ByteString
toPathString Mainnet = "live"
toPathString Testnet3 = "test"



