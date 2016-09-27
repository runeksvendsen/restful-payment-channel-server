{-# LANGUAGE FlexibleInstances #-}

module ChanStore.Lib.ChanMap where


import           Data.DiskMap (newDiskMap, SyncAction,
                            addItem, getItem,
                            CreateResult(..),
                            MapItemResult(..),getResult,
                            mapGetItem, updateIfRight,
                            getItemCount,
                            getFilteredKeys,
                            makeReadOnly)

import           Common.Types
import           ChanStore.Lib.Types
import           PayChanServer.Config.Types (ServerDBConf(..))

import           Data.Bitcoin.PaymentChannel.Types
import           Data.Bitcoin.PaymentChannel.Movable

import           Control.Concurrent.STM (STM, atomically)
import           Control.Monad.Catch (finally, handleAll, Exception)
import           Control.Concurrent (forkIO, killThread, threadDelay)

import           Data.Maybe (fromMaybe)


logImportantError = putStrLn

-- Create/destroy
createChanMap :: ServerDBConf -> IO ChannelMap
createChanMap (ServerDBConf syncDir syncInterval) =
    if syncInterval == 0 then do
        (map,Nothing) <- newDiskMap syncDir False
        return $ ChannelMap map Nothing
    else do
        (map,Just syncAction) <- newDiskMap syncDir True
        putStrLn $ "INFO: Deferred sync enabled. Syncing every " ++
            show syncInterval ++ " seconds."
        threadId <- forkIO $ syncThread syncAction syncInterval
        return $ ChannelMap map $ Just (syncAction,threadId)

destroyChanMap :: ChannelMap -> IO ()
destroyChanMap (ChannelMap chanMap (Just (syncAction,syncThreadId))) = do
    makeReadOnly chanMap
    killThread syncThreadId
    syncNow syncAction
destroyChanMap (ChannelMap _ Nothing) = return ()

-- Storage interface
addChanState :: ChannelMap -> OpenRequest -> IO OpenResult
addChanState (ChannelMap diskMap _) (OpenRequest cp fti fp) =
    either
        (return . OpenError)
        (\(amt, mc, valLeft) ->
            addItem diskMap key (ReadyForPayment mc NoPayload) >>=
            (\res -> case res of
                    Created         -> return $ ChannelOpened amt valLeft
                    AlreadyExists   -> return   ChannelExists))
        (newMovableChan cp fti fp)
    where key = cpSenderPubKey cp

registerPayment :: ChannelMap -> PayRequest -> IO PayResult
registerPayment (ChannelMap diskMap _) (PayRequest key fp) =
    let
        updFunc :: ChanState -> Either PayResult (ChanState, PayResult)
        -- Success
        updFunc (ReadyForPayment mc pl)   =
            case recvSinglePayment mc fp of
                Right (valRecvd, newMC, valLeft) ->
                        Right   ( ReadyForPayment newMC NoPayload
                                , PaymentReceived valRecvd valLeft (getDataPayload pl valRecvd)
                                )
                Left payChanErr    ->
                        Left $ PaymentError payChanErr
        -- Error
        updFunc (ChannelSettled txid _ (rpc,_)) =
            Left $ PayUpdateError $ ChanClosed txid (channelValueLeft rpc)
        updFunc SettlementInProgress{} = Left $ PayUpdateError ChanBeingClosed
        checkMapUpdRes res             =
            fromMaybe (PayUpdateError NoSuchChannel) (getResult res)
    in
        checkMapUpdRes <$> updateIfRight diskMap key updFunc

closeBegin :: ChannelMap -> CloseBeginRequest -> IO CloseBeginResult
closeBegin (ChannelMap diskMap _) (CloseBeginRequest (ChannelResource clientPK lt op) sig) =
    let
        updFunc :: ChanState -> Either CloseBeginResult (ChanState, CloseBeginResult)
        updFunc (ReadyForPayment mc _)   = case getStateByInfo mc lt op of
                Nothing      -> Left $ CloseUpdateError NoSuchChannel
                Just (rpc,v) -> checkRpcSig rpc >>
                    Right (SettlementInProgress (rpc,v),
                           CloseInitiated       (rpc,v))
                where checkRpcSig rpc =
                        if getNewestSig rpc == sig then Right () else Left IncorrectSig
        updFunc (ChannelSettled txid _ (rpc,_)) =
            Left $ CloseUpdateError $ ChanClosed txid (channelValueLeft rpc)
        updFunc SettlementInProgress{} = Left $ CloseUpdateError ChanBeingClosed
        checkMapUpdRes res             = fromMaybe (CloseUpdateError NoSuchChannel) (getResult res)
    in
        checkMapUpdRes <$> updateIfRight diskMap clientPK updFunc


-- |Management
registerDataPayload :: ChannelMap -> DataPayloadRequest -> IO DataPayloadResult
registerDataPayload (ChannelMap diskMap _) (DataPayloadRequest key pl amt) =
    let
        updFunc :: ChanState -> Either DataPayloadResult (ChanState, DataPayloadResult)
        updFunc (ReadyForPayment mc _)   =
            Right (ReadyForPayment mc (PayloadAtPrice pl amt), OK)
        updFunc (ChannelSettled txid _ (rpc,_)) =
            Left $ RegisterPayloadError $ ChanClosed txid (channelValueLeft rpc)
        updFunc SettlementInProgress{} = Left $ RegisterPayloadError ChanBeingClosed
        checkMapUpdRes res             =
            fromMaybe (RegisterPayloadError NoSuchChannel) (getResult res)
    in
        checkMapUpdRes <$> updateIfRight diskMap key updFunc

mapLen (ChannelMap diskMap _) = getItemCount diskMap

openChannelCount :: ChannelMap -> IO Int
openChannelCount (ChannelMap chanMap _) = atomically $
    length <$> getFilteredKeys chanMap isOpen


syncNow :: SyncAction -> IO ()
syncNow syncAction = do
    numItems <- syncAction
    putStrLn $ "Synced " ++ show numItems ++ " to disk"

syncThread ::
    SyncAction
    -> Word -- ^ Sync interval in seconds
    -> IO ()
syncThread syncAction delaySecs =
    let delayMicroSecs = fromIntegral delaySecs * round 1e6 in
    (handleAll  -- Log errors from syncing
        ( undefined )
        ( do
              threadDelay delayMicroSecs
              syncNow syncAction
        )
    )
    `finally` (    -- Regardless of what happens, keep thread running
        syncThread syncAction delaySecs )

-- handleSyncThreadException :: Exception e => e -> IO ()
-- handleSyncThreadException e = case asyncExceptionFromException e of
--     ThreadKilled    -> return ()
--     e               -> logImportantError $
--         "FATAL: Exception while syncing to disk: " ++ show e

-- Helper functions
isSettled :: ChanState -> Bool
isSettled (ChannelSettled _ _ _) = True
isSettled _                      = False

isOpen :: ChanState -> Bool
isOpen (ReadyForPayment _ _) = True
isOpen _                   = False
