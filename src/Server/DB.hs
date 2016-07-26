module Server.DB where

import           Server.Util
import qualified ChanStoreServer.Interface as DBConn
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment,
                                                    PaymentChannel(getNewestPayment))
import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString as BS
import           Snap
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (try)
import           Network.HTTP.Client (HttpException (..))
import           Data.EitherR (fmapL)


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

confirmChannelDoesntExistOrAbort :: MonadSnap m => DBConn.ConnManager -> BS.ByteString -> HT.OutPoint -> m ()
confirmChannelDoesntExistOrAbort chanMap basePath chanId = do
    maybeItem <- tryDBRequest (DBConn.chanGet chanMap chanId)
    case fmap DBConn.isSettled maybeItem of
        Nothing -> return ()    -- channel doesn't already exist
        Just False ->           -- channel exists already, and is open
            httpLocationSetActiveChannel basePath chanId >>
            errorWithDescription 409 "Channel already exists"
        Just True  ->           -- channel in question has been settled
            errorWithDescription 409 "Channel already existed, but has been settled"


getChannelStateOr404 :: MonadSnap m => DBConn.ConnManager -> HT.OutPoint -> m DBConn.ChanState
getChannelStateOr404 chanMap chanId =
    tryDBRequest (DBConn.chanGet chanMap chanId) >>=
    \res -> case res of
        Nothing ->
            errorWithDescription 404 "No such channel"
        Just cs -> return cs

getChannelStateForPayment :: MonadSnap m => DBConn.ConnManager -> HT.OutPoint -> m ReceiverPaymentChannel
getChannelStateForPayment chanMap chanId =
    getChannelStateForSettlement chanMap chanId >>=
    -- When the channel has changed from ReadyForPayment to
    --  SettlementInProgress, the "/pay" resource is gone
    either (const $ errorWithDescription 404 "No such payment resource") undefined

-- |Return either open ChanState or settlement txid and most recent payment in case
--      the channel is closed
getChannelStateForSettlement :: MonadSnap m => DBConn.ConnManager -> HT.OutPoint -> m (Either (HT.TxHash,Payment) ReceiverPaymentChannel)
getChannelStateForSettlement chanMap chanId =
    getChannelStateOr404 chanMap chanId >>=
    \chanState -> case chanState of
        (DBConn.ReadyForPayment rpc) ->
            return $ Right rpc
        (DBConn.SettlementInProgress _) ->
            errorWithDescription 410 "Channel is being closed"
        (DBConn.ChannelSettled txid paym _) ->
            return $ Left (txid,paym)

