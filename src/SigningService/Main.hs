{-# LANGUAGE OverloadedStrings #-}

module SigningService.Main where

import           Prelude hiding (init, userError)

import           SigningService.Types
import           SigningService.Util (produceSettlementTx)

import           PayChanServer.Main (wrapArg)
import           PayChanServer.Config.Util (loadConfig, configLookupOrFail, setBitcoinNetwork, getSigningSettleConfig)
import qualified PayChanServer.Config.Util as Conf (Config)
import           PayChanServer.Util (writeBinary, decodeFromBody, userError, getQueryArg)
import           PayChanServer.Types (SigningSettleConfig(..))
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount, RecvPubKey(MkRecvPubKey))

import           PayChanServer.Init (installHandlerKillThreadOnSig)
import           Control.Concurrent (myThreadId)
import qualified Data.ByteString as BS
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified System.FilePath as F
import qualified System.Posix.Signals as Sig
import           Snap
import           Control.Lens (use)
import           Control.Monad.IO.Class (liftIO)


site :: [(BS.ByteString, Handler AppConf AppConf ())]
site = [
        ("/settle_channel", method POST $ parseRequest >>= createSignTx >>= writeBinary)
    ,   ("/get_pubkey",     method GET  $ use pubKey   >>= writeBinary)
    ]

parseRequest :: Handler AppConf AppConf (ReceiverPaymentChannel,BitcoinAmount)
parseRequest = do
    chanState <- decodeFromBody 1024
    txFee <- getQueryArg "tx_fee"
    return (chanState,txFee)

createSignTx :: (ReceiverPaymentChannel,BitcoinAmount) -> Handler AppConf AppConf HT.Tx
createSignTx (chanState,txFee) = do
    mkSettlementTx <- use makeSettlementTxFunc
    return $ mkSettlementTx (chanState,txFee)

init :: Conf.Config -> SnapletInit AppConf AppConf
init cfg = makeSnaplet "SigningService" "Settlement transaction producer" Nothing $ do
    settleConfig@(SigningSettleConfig privKey _) <- liftIO $ getSigningSettleConfig cfg
    let confPubKey = MkRecvPubKey $ HC.derivePubKey privKey
    addRoutes $ site
    return $ AppConf confPubKey $ produceSettlementTx settleConfig

main :: IO ()
main = wrapArg $ \cfg cfgFilePath -> do
    configLookupOrFail cfg "bitcoin.network" >>= setBitcoinNetwork
    port <- configLookupOrFail cfg "network.port"
    mainThread <- myThreadId
    _ <- installHandlerKillThreadOnSig Sig.sigTERM mainThread
    run port (F.dropExtension cfgFilePath) cfg


run :: Word -> String -> Conf.Config -> IO ()
run port env cfg = do
    (_, app, _) <- runSnaplet (Just env) (init cfg)
    setPort (fromIntegral port) defaultConfig `httpServe` app
