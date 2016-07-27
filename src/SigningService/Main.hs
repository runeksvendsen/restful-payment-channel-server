{-# LANGUAGE OverloadedStrings #-}

module SigningService.Main where

import           Prelude hiding (init, userError)

import           SigningService.Types
import           SigningService.Util (produceSettlementTx)

import           Server.Main (wrapArg)
import           Server.Config (loadConfig, configLookupOrFail, getSigningSettleConfig,
                                getBitcoindConf, setBitcoinNetwork)
import qualified Server.Config as Conf (Config)
import           Server.Util (writeResponseBody, decodeFromBody,
                              internalError, userError, getPathArg, getQueryArg, getOptionalQueryArg,
                              errorWithDescription)
import           Server.Types (SigningSettleConfig(..))
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount)

import           Server.Init (installHandlerKillThreadOnSig)
import           Control.Concurrent (myThreadId)
import qualified Data.ByteString as BS
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified System.FilePath as F
import qualified System.Posix.Signals as Sig
import           Snap
import           Snap.Http.Server (defaultConfig, httpServe)
import           Control.Lens (use)
import           Control.Monad.IO.Class (liftIO, MonadIO)



site :: [(BS.ByteString, Handler AppConf AppConf ())]
site = [
        ("/settle_channel", method POST $ parseSigningRequest >>= writeResponseBody)
    ,   ("/get_pubkey",     method GET $  use pubKey >>= writeResponseBody)
    ]

parseSigningRequest :: Handler AppConf AppConf (ReceiverPaymentChannel,BitcoinAmount)
parseSigningRequest = do
    eitherState <- decodeFromBody 1024
    txFee <- getQueryArg "tx_fee"
    case eitherState of
        Left e -> userError $ "Failed to parse ReceiverPaymentChannel from body: " ++ e
        Right chanState -> return (chanState,txFee)

writeSigningResponse :: (ReceiverPaymentChannel,BitcoinAmount) -> Handler AppConf AppConf HT.Tx
writeSigningResponse (chanState,txFee) = do
    mkSettlementTx <- use makeSettlementTxFunc
    return $ mkSettlementTx (chanState,txFee)

init :: Conf.Config -> SnapletInit AppConf AppConf
init cfg = makeSnaplet "SigningService" "Settlement transaction producer" Nothing $ do
    settleConfig@(SigningSettleConfig privKey _) <- liftIO $ getSigningSettleConfig cfg
    let confPubKey = HC.derivePubKey privKey
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
