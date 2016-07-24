{-# LANGUAGE OverloadedStrings #-}

module SigningService.Main where

import           Prelude hiding (init, userError)

import           SigningService.Types
import qualified SigningService.Spec as Spec (basePath)

import           Server.Main (wrapArg)
import           Server.Config (loadConfig, configLookupOrFail, getSigningSettleConfig,
                                getBitcoindConf, setBitcoinNetwork)
import qualified Server.Config as Conf (Config)
import           Server.Util (reqBoundedData, writeBinary,
                              internalError, userError, getPathArg, getQueryArg, getOptionalQueryArg,
                              errorWithDescription)
import           Server.ChanStore.Settlement (produceSettlementTx)

import           Data.Bitcoin.PaymentChannel ()
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..), Payment)

import           Bitcoind
import qualified Network.Haskoin.Transaction as HT

import           Snap -- (serveSnaplet, makeSnaplet, SnapletInit, Snap, Method(..))
import           Snap.Http.Server (defaultConfig, httpServe)
import           Control.Applicative ((<|>))

import           Control.Lens (use)
import           Control.Monad (unless, forM_)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Catch (bracket, finally, try)
import qualified System.FilePath as F
import qualified Control.Exception as E


site :: Handler AppConf AppConf ()
site = route [
        (Spec.basePath, method POST $ parseRequest >>= writeResponse)
    ]

parseRequest :: Handler AppConf AppConf (ReceiverPaymentChannel,BitcoinAmount)
parseRequest = do
    eitherState <- reqBoundedData 1024
    txFee <- getQueryArg "tx_fee"
    case eitherState of
        Left e -> userError $ "Failed to parse ReceiverPaymentChannel from body: " ++ e
        Right chanState -> return (chanState,txFee)

writeResponse :: (ReceiverPaymentChannel,BitcoinAmount) -> Handler AppConf AppConf ()
writeResponse (chanState,txFee) = do
    mkSettlementTx <- use makeSettlementTxFunc
    let writeSettlementTx = either (internalError . show) writeBinary
    writeSettlementTx $ mkSettlementTx (chanState,txFee)

init :: Conf.Config -> SnapletInit AppConf AppConf
init cfg = makeSnaplet "SigningService" "Settlement transaction producer" Nothing $ do
    settleConfig <- liftIO $ getSigningSettleConfig cfg
    return $ AppConf $ produceSettlementTx settleConfig

main :: IO ()
main = wrapArg $ \cfg cfgFilePath -> do
    configLookupOrFail cfg "bitcoin.network" >>= setBitcoinNetwork
    port <- configLookupOrFail cfg "network.port"
    run port (F.dropExtension cfgFilePath) cfg

run :: Word -> String -> Conf.Config -> IO ()
run port env cfg = do
    (_, app, _) <- runSnaplet (Just env) (init cfg)
    setPort (fromIntegral port) defaultConfig `httpServe` app





errorOnException :: MonadSnap m => E.IOException -> m ()
errorOnException = internalError . show
