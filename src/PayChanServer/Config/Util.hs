{-# LANGUAGE OverloadedStrings #-}

module  PayChanServer.Config.Util
(
loadConfig,configLookupOrFail,
getChanOpenConf,getServerSettleConfig,getSigningSettleConfig,
getBitcoindConf,getSigningServiceConn,
BitcoinNet,
setBitcoinNetwork,
getDBConf,
getServerDBConf,connFromDBConf,
configDebugIsEnabled,
-- re-exports
Config

)
where

import           PayChanServer.Config.Types
import           Common.Util
import           ConnManager.Connection (newConnManager)
import           Common.Util (fromHexString)
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Constants as HCC
import qualified Crypto.Secp256k1 as Secp

import           Control.Applicative (liftA2)
import           Control.Lens.TH (makeLenses)
import qualified Data.ByteString as BS
import           Data.Ratio
import           Data.Configurator.Types
import qualified Data.Configurator as Conf
import           Data.String.Conversions (cs)
import           ChanStore.Lib.Types (ConnManager)
import           PayChanServer.Types
import           Bitcoind (BTCRPCInfo(..))



-- |Optional. If set to True, bypasses funding/settlement in order to enable testing
configDebugIsEnabled :: Config -> IO Bool
configDebugIsEnabled cfg =
    Conf.lookupDefault False cfg "debug.enable"

loadConfig :: String -> IO Config
loadConfig confFile = Conf.load [Conf.Required confFile]

configLookupOrFail :: Configured a => Config -> Name -> IO a
configLookupOrFail conf name =
    Conf.lookup conf name >>= maybe
        (fail $ "ERROR: Failed to read key \"" ++ cs name ++
            "\" in config (key not present or invalid)")
        return

connFromDBConf :: DBConf -> IO ConnManager
connFromDBConf (DBConf host port numConns) = newConnManager host port numConns

getDBPath :: Config -> IO FilePath
getDBPath cfg = configLookupOrFail cfg "storage.stateDir"

getDBSyncInterval :: Config -> IO Word
getDBSyncInterval cfg = Conf.lookupDefault 0 cfg "storage.deferredSyncInterval"

getServerDBConf :: Config -> IO ServerDBConf
getServerDBConf cfg = ServerDBConf <$>
    getDBPath cfg <*>
    getDBSyncInterval cfg

getSigningServiceConn :: Config -> IO ConnManager
getSigningServiceConn cfg = do
    host <- (configLookupOrFail cfg "settlement.signingService.host")
    port <- (configLookupOrFail cfg "settlement.signingService.port")
    newConnManager host port 1

getDBConf :: Config -> IO DBConf
getDBConf cfg = DBConf <$>
    configLookupOrFail cfg "chanStore.host" <*>
    configLookupOrFail cfg "chanStore.port" <*>
    configLookupOrFail cfg "chanStore.clientConnPoolSize"

getChanOpenConf :: Config -> IO OpenConfig
getChanOpenConf cfg = OpenConfig <$>
    configLookupOrFail cfg "open.fundingTxMinConf" <*>
    configLookupOrFail cfg "open.basePrice" <*>
    configLookupOrFail cfg "open.priceAddSettlementFee" <*>
    configLookupOrFail cfg "open.minDurationHours"

getServerSettleConfig :: Config -> IO ServerSettleConfig
getServerSettleConfig cfg = ServerSettleConfig <$>
        fmap calcSettlementFeeSPB (configLookupOrFail cfg "settlement.txFeeSatoshiPerByte") <*>
        configLookupOrFail cfg "settlement.settlementPeriodHours"

-- |For SigningService
getSigningSettleConfig :: Config -> IO SigningSettleConfig
getSigningSettleConfig cfg = SigningSettleConfig <$>
        configLookupOrFail cfg "settlement.privKeySeed" <*>
        configLookupOrFail cfg "settlement.fundsDestinationAddress"

getBitcoindConf :: Config -> IO BTCRPCInfo
getBitcoindConf cfg = BTCRPCInfo <$>
    configLookupOrFail cfg "bitcoin.bitcoindRPC.ip" <*>
    configLookupOrFail cfg "bitcoin.bitcoindRPC.port" <*>
    configLookupOrFail cfg "bitcoin.bitcoindRPC.user" <*>
    configLookupOrFail cfg "bitcoin.bitcoindRPC.pass" <*>
    configDebugIsEnabled cfg

-- | Roughly accurate (±10%-ish), because we know the two possible sizes (one/two outputs)
--  of the settlement transaction. Could use refinement.
calcSettlementFeeSPB :: BitcoinAmount -> BitcoinAmount
calcSettlementFeeSPB satoshisPerByte = 331 * satoshisPerByte

setBitcoinNetwork :: BitcoinNet -> IO ()
setBitcoinNetwork Mainnet = return ()
setBitcoinNetwork Testnet3 = HCC.switchToTestnet3
