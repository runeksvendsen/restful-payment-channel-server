{-# LANGUAGE OverloadedStrings #-}

module SettlementService.Main where

import           Prelude hiding (init, userError)

import           SettlementService.Types
import qualified SettlementService.Spec as Spec (basePath)

import           Server.Main (wrapArg)
import           Server.Config (loadConfig, configLookupOrFail, getSettleConfig,
                                getBitcoindConf, setBitcoinNetwork)
import qualified Server.Config as Conf (Config)
import           Server.Util (reqBoundedData, writeBinary,
                              internalError, userError, getPathArg, getQueryArg, getOptionalQueryArg,
                              errorWithDescription)
import           Server.ChanStore.Settlement (settleChannelEither)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..), Payment)

import           Bitcoind
import qualified Network.Haskoin.Transaction as HT

import           Snap -- (serveSnaplet, makeSnaplet, SnapletInit, Snap, Method(..))
import           Snap.Http.Server (defaultConfig, httpServe)
import           Control.Applicative ((<|>))

import           Control.Lens (use)
import           Control.Monad (unless, forM)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Catch (bracket, finally, try)
import qualified System.FilePath as F
import qualified Control.Exception as E


site :: Handler AppConf AppConf ()
site = route [
    (Spec.basePath, method POST pushTx) ]

pushTx :: Handler AppConf AppConf ()
pushTx = do
    settleChan <- use settleChannelFunc
    eitherStates <- reqBoundedData 512
    case eitherStates of
        Right chanStateList -> forM chanStateList (\chanState ->
            liftIO (settleChan chanState)) >>= either internalError writeBinary
        Left e -> userError $ "Failed to parse ReceiverPaymentChannel from body: " ++ e

init :: Conf.Config -> SnapletInit AppConf AppConf
init cfg = makeSnaplet "SettlementService" "Push Bitcoin tx to the network" Nothing $ do
    settleConfig <- liftIO $ getSettleConfig cfg
    bitcoindRPCConf <- liftIO $ getBitcoindConf cfg
    return $ AppConf $ settleChannelEither bitcoindRPCConf settleConfig

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
