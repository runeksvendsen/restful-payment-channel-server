{-# LANGUAGE  ScopedTypeVariables #-}

module  PayChanServer.DB where

import           Common.Util
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf

import qualified ChanStore.Interface as DBConn
import qualified ChanStore.Lib.ChanMap as ChanMap
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment,
                                                    PaymentChannel(getNewestPayment))
import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString as BS
import           Snap
import           Control.Monad.IO.Class (liftIO)
import           Control.Lens (use)
import           Control.Exception (try, throwIO)
import           Control.Concurrent (threadDelay)
import           Network.HTTP.Client (HttpException (FailedConnectionException, FailedConnectionException2))
import           Data.EitherR (fmapL)
import           System.IO (hFlush, stdout)
import           Text.Printf (printf)


-- |Keep trying to connect (only used during startup).
--  If we fail to get a result because host is offline:
--   log about it, then keep trying every 'delaySecs'
--   seconds until we get a result.
initWaitConnect :: String -> IO a -> IO a
initWaitConnect serviceName ioa = do
    let delaySecs = 0.1
    let isFailedConnectOrThrow e =
            case e of
                FailedConnectionException  host port     -> return (host,port)
                FailedConnectionException2 host port _ _ -> return (host,port)
                _ -> throwIO e
    -- Keep trying to connect
    let loopGet getter = do
            eitherError <- getter
            case eitherError of
                Left (e :: HttpException) -> do
                        isFailedConnectOrThrow e
                        threadDelay (fromIntegral $ round $ delaySecs * 1e6)
                        loopGet getter
                Right res -> return res
    let logNoConnection (host,port) = putStr $ printf
             "%s offline (%s:%d). Waiting for %s to come online... "
             serviceName host port serviceName
    let tryAsEither = try ioa
    tryAsEither >>= either  -- <- First try to connect
        -- If there's no connection: log; make it print everything to stdout; wait in loop
        (\e -> isFailedConnectOrThrow e >>= logNoConnection >> hFlush stdout >> loopGet tryAsEither)
        return

tryDBRequest :: MonadSnap m => IO a -> m a
tryDBRequest ioa = liftIO (tryHTTPRequestOfType "Database" ioa) >>=
                       either internalError return

trySigningRequest :: MonadSnap m => IO a -> m a
trySigningRequest ioa =
    liftIO (tryHTTPRequestOfType "Signing service" ioa) >>=
        either internalError return

tryHTTPRequestOfType :: String -> IO a -> IO (Either String a)
tryHTTPRequestOfType descr ioa =
    fmapL (\e -> descr ++ " error: " ++ show (e :: HttpException)) <$> try ioa

confirmChannelDoesntExistOrAbort :: DBConn.Interface -> HT.OutPoint -> Handler Conf.App Conf.App ()
confirmChannelDoesntExistOrAbort chanMap chanId = do
    maybeItem <- tryDBRequest (DBConn.chanGet chanMap chanId)
    case fmap ChanMap.isSettled maybeItem of
        Nothing -> return ()    -- channel doesn't already exist
        Just False ->           -- channel exists already, and is open
            httpLocationSetActiveChannel chanId >>
            errorWithDescription 409 "Channel already exists"
        Just True  ->           -- channel in question has been settled
            errorWithDescription 409 "Channel already existed, but has been settled"


getChannelStateOr404 :: MonadSnap m => DBConn.Interface -> HT.OutPoint -> m DBConn.ChanState
getChannelStateOr404 chanMap chanId =
    tryDBRequest (DBConn.chanGet chanMap chanId) >>=
    \res -> case res of
        Nothing ->
            errorWithDescription 404 "No such channel"
        Just cs -> return cs

getChannelStateForPayment :: MonadSnap m => DBConn.Interface -> HT.OutPoint -> m ReceiverPaymentChannel
getChannelStateForPayment chanMap chanId =
    getChannelStateForSettlement chanMap chanId >>=
    -- When the channel has changed from ReadyForPayment to
    --  SettlementInProgress, the "/pay" resource is gone
    either (const $ errorWithDescription 404 "No such payment resource") return

-- |Return either open ChanState or settlement txid and most recent payment in case
--      the channel is closed
getChannelStateForSettlement :: MonadSnap m => DBConn.Interface -> HT.OutPoint -> m (Either (HT.TxHash,Payment) ReceiverPaymentChannel)
getChannelStateForSettlement chanMap chanId =
    getChannelStateOr404 chanMap chanId >>=
    \chanState -> case chanState of
        (DBConn.ReadyForPayment rpc) ->
            return $ Right rpc
        (DBConn.SettlementInProgress _) ->
            errorWithDescription 410 "Channel is being closed"
        (DBConn.ChannelSettled txid paym _) ->
            return $ Left (txid,paym)

