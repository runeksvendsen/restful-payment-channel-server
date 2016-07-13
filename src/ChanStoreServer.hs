{-# LANGUAGE OverloadedStrings #-}

module ChanStoreServer where

import           Server.ChanStore
import           Server.ChanStore.ChanStore
import           Server.Main (wrapArg)
import           Server.Util (internalError, userError, getPathArg)

import           Snap -- (serveSnaplet)
import           Snap.Http.Server -- (defaultConfig, httpServe)
import           Control.Applicative ((<|>))
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..))
import           Data.Bitcoin.PaymentChannel.Util (unsafeUpdateRecvState)
import           Control.Monad.Catch (bracket, finally, try)

main :: IO ()
main = wrapArg $ \cfgFilePath -> do
    putStrLn $ "Using config file " ++ show cfgFilePath ++ ". Reading..."
    cfg <- loadConfig cfgFilePath
    chanStoreDir <- configLookupOrFail cfg "storage.stateDir"
    map <- init_chanMap chanStoreDir
    quickHttpServe $ site map

site :: Snap ()
site map =
    route [ ("/channels/"
             ,      method POST   $ create map) -- ReceiverPaymentChannel)

          , ("/channels/:funding_outpoint"
             ,      method GET    $ get map
                <|> method PUT    $ update map
                <|> method DELETE $ settle map) ]

create map = do
    eitherState <- reqBoundedData 256
    case eitherState of
        Right newChanState ->
            let key = getChannelID newChanState in liftIO $
                try (addChanState map key newChanState) >>=
                    either errorOnException (const $ return ())
        Left e -> userError e

get map = do
    outPoint <- getPathArg "funding_outpoint"
    maybeItem <- getChanState map outPoint
    case maybeItem of
        Nothing -> errorWithDescription 404 "No such channel"
        Just item -> writeBinary item

update map = do
    outPoint <- getPathArg "funding_outpoint"
    eitherPayment <- reqBoundedData 80
    case eitherPayment of
        Right payment -> updateWithPayment map outPoint payment
        Left e -> userError e

settle map = do
    outPoint    <- getPathArg  "funding_outpoint"
    settleTxId  <- getQueryArg "settlement_txid"
    itemExists  <- chanDelete map outPoint settleTxId
    case itemExists of
        True  -> return ()
        False -> errorWithDescription 404 "No such channel"


updateWithPayment :: MonadSnap m => ChannelMap -> OutPoint -> Payment -> m ()
updateWithPayment  map outPoint payment = do
    maybeItem <- getChanState map outPoint
    case maybeItem of
        Nothing ->
            errorWithDescription 404 "No such channel"
        Just (ReadyForPayment oldState) -> liftIO $
            updateChanState map outPoint newState
                where newState = unsafeUpdateRecvState oldState payment
        Just (ChannelSettled _) ->
            userError "Channel is settled"
        Just (SettlementInProgress _) ->
            userError "Channel is being settled"

errorOnException :: MonadSnap m => Exception -> m ()
errorOnException = internalError . show