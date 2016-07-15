{-# LANGUAGE OverloadedStrings #-}

module ChanStoreServer where

import           Prelude hiding (userError)

import           Server.ChanStore
import           Server.ChanStore.Types
import           Server.ChanStore.ChanStore
import           Server.Main (wrapArg)
import           Server.Config (Config, loadConfig, configLookupOrFail, getSettleConfig,
                                getBitcoindConf, setBitcoinNetwork)
import           Server.Util (reqBoundedData, writeBinary,
                              internalError, userError, getPathArg, getQueryArg, getOptionalQueryArg,
                              errorWithDescription)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..), Payment)

import qualified Network.Haskoin.Transaction as HT

import           Snap -- (serveSnaplet)
import           Snap.Http.Server -- (defaultConfig, httpServe)
import           Control.Applicative ((<|>))

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Catch (bracket, finally, try)
import           Control.Concurrent.STM (STM, atomically)
import qualified Control.Exception as E


main :: IO ()
main = wrapArg $ \cfgFilePath -> do
    putStrLn $ "Using config file " ++ show cfgFilePath ++ ". Reading..."
    cfg <- loadConfig cfgFilePath

    configLookupOrFail cfg "bitcoin.network" >>= setBitcoinNetwork

    map <- init_chanMap =<< configLookupOrFail cfg "storage.stateDir"
    port <- configLookupOrFail cfg "network.port"

    let conf = setPort (fromIntegral (port :: Word)) defaultConfig
    httpServe conf $ site map


site :: ChannelMap -> Snap ()
site map =
    route [ ("/channels/"
             ,      method POST   $ create map)

          , ("/channels/:funding_outpoint"
             ,      method GET    ( get map   )
                <|> method PUT    ( update map)
                <|> method DELETE ( settle map) )

            -- management/settlement interface (retrieve a list of open channels)
          , ("/channels/all"
             ,      method GET    ( getAll map ))]

create :: ChannelMap -> Snap ()
create map = do
    eitherState <- reqBoundedData 256
    case eitherState of
        Right newChanState ->
            let key = getChannelID newChanState in
                liftIO (try $ addChanState map key newChanState) >>=
                    either errorOnException (const $ return ())
        Left e -> userError e

get :: ChannelMap -> Snap ()
get map = do
    outPoint <- getPathArg "funding_outpoint"
    maybeItem <- liftIO $ getChanState map outPoint
    case maybeItem of
        Nothing -> errorWithDescription 404 "No such channel"
        Just item -> writeBinary item

update :: ChannelMap -> Snap ()
update map = do
    outPoint <- getPathArg "funding_outpoint"
    eitherPayment <- reqBoundedData 80
    case eitherPayment of
        Right payment -> updateWithPayment map outPoint payment
        Left e -> userError e

settle :: ChannelMap -> Snap ()
settle map = do
    outPoint    <- getPathArg  "funding_outpoint"
    settleTxId  <- getQueryArg "settlement_txid"
    itemExisted  <- liftIO $ deleteChanState map outPoint settleTxId
    case itemExisted of
        True  -> return ()
        False -> errorWithDescription 404 "No such channel"

getAll :: ChannelMap -> Snap ()
getAll m = do
    maybeExpiresEarlier <- getOptionalQueryArg "expires_before"
    let filterFunc = case maybeExpiresEarlier of
            Just exp -> \i -> getExpirationDate (csState i) < exp
            Nothing  -> \_ -> True
    liftIO (getFilteredChanStates m filterFunc) >>= writeBinary

updateWithPayment :: MonadSnap m => ChannelMap -> HT.OutPoint -> Payment -> m ()
updateWithPayment  map chanId payment = do
    success <- liftIO $ updateChanState map chanId payment
    unless success $
        userError "Not an open channel"

errorOnException :: MonadSnap m => E.IOException -> m ()
errorOnException = internalError . show
