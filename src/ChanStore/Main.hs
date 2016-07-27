{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChanStore.Main where

import           Prelude hiding (userError)

import           ChanStore.Lib.Types
import           ChanStore.Init (init_chanMap)
import           ChanStore.Lib.ChanMap
import           ChanStore.Lib.Settlement (beginSettlingExpiringChannels, beginSettlingChannel,
                                                       finishSettlingChannel)
import           PayChanServer.Main (wrapArg)
import           PayChanServer.Config.Util (Config, loadConfig, configLookupOrFail,
                                            setBitcoinNetwork, getDBPath)
import           PayChanServer.Util (decodeFromBody, writeBinary,
                              internalError, userError, getPathArg, getQueryArg, getOptionalQueryArg,
                              errorWithDescription)
import           PayChanServer.Init (installHandlerKillThreadOnSig)
import           Control.Concurrent (myThreadId)
import qualified System.Posix.Signals as Sig

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
main = wrapArg $ \cfg _ -> do
    configLookupOrFail cfg "bitcoin.network" >>= setBitcoinNetwork
    port <- configLookupOrFail cfg "network.port"
    let conf = setPort (fromIntegral (port :: Word)) defaultConfig
    mainThread <- myThreadId
    _ <- installHandlerKillThreadOnSig Sig.sigTERM mainThread
    --       1. first do this            3. at the end always do this
    bracket
        (init_chanMap =<< getDBPath cfg)
        (const $ return ())
        (\map -> httpServe conf $ site map)


site :: ChannelMap -> Snap ()
site map =
    route [
            -- store/db
            ("/store/by_id/"
             ,      method POST   $ create map >>= writeBinary)

          , ("/store/by_id/:funding_outpoint"
             ,      method GET    ( get map    >>= writeBinary)
                <|> method PUT    ( update map >>= writeBinary) )
--                 <|> method DELETE ( settle map >>= writeResponse) )

            -- expiring channels management/settlement interface
          , ("/settlement/begin/by_exp/:expiring_before"
             ,      method PUT    ( settleByExp map >>= writeBinary ))
          , ("/settlement/begin/by_id/:funding_outpoint"
             ,      method PUT    ( settleByKey map >>= writeBinary ))

          , ("/settlement/finish/by_id/:funding_outpoint"
             ,      method POST   ( settleFin map >>= writeBinary ))]

create :: ChannelMap -> Snap CreateResult
create map = do
    newChanState <- decodeFromBody 1024
    let key = getChannelID newChanState
    tryDBRequest $ addChanState map key newChanState


get :: ChannelMap -> Snap ChanState
get map = do
    outPoint <- getPathArg "funding_outpoint"
    maybeItem <- liftIO $ getChanState map outPoint
    case maybeItem of
        Nothing -> errorWithDescription 404 "No such channel"
        Just item -> return item

update :: ChannelMap -> Snap UpdateResult
update map = do
    outPoint <- getPathArg "funding_outpoint"
    payment <- decodeFromBody 128
    liftIO (updateChanState map outPoint payment) >>=
        (\exists -> case exists of
                ItemUpdated _ _  -> return WasUpdated
                NotUpdated       -> return WasNotUpdated
                NoSuchItem       -> errorWithDescription 404 "No such channel"
        )

settleByKey :: ChannelMap -> Snap ReceiverPaymentChannel
settleByKey m = do
    key <- getPathArg "funding_outpoint"
    liftIO $ beginSettlingChannel m key

settleByExp :: ChannelMap -> Snap [ReceiverPaymentChannel]
settleByExp m = do
    settlementTimeCutoff <- getPathArg "expiring_before"
    liftIO $ beginSettlingExpiringChannels m settlementTimeCutoff

settleFin :: ChannelMap -> Snap ()
settleFin m = do
    key <- getPathArg "funding_outpoint"
--     bs <- readRequestBody 2048
--     liftIO $ print bs
    settleTxId <- decodeFromBody 32
    liftIO $ print (settleTxId :: HT.TxHash)
    res <- tryDBRequest $ finishSettlingChannel m (key,settleTxId)
    case res of
        (ItemUpdated _ _) -> return ()
        NotUpdated -> userError $ "Channel isn't in the process of being settled." ++
                                  " Did you begin settlement first?" ++
                                  " Also, are you sure you have the right key?"
        NoSuchItem -> errorWithDescription 404 "No such channel"







---- Util --
tryDBRequest :: MonadSnap m => IO a -> m a
tryDBRequest ioa = either errorOnException return =<< liftIO (try ioa)

errorOnException :: MonadSnap m => E.IOException -> m a
errorOnException = internalError . show


