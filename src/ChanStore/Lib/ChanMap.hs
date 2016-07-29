{-# LANGUAGE FlexibleInstances #-}

module ChanStore.Lib.ChanMap where


import           DiskMap (newDiskMap, SyncAction,
                            addItem, getItem,
                            CreateResult,
                            mapGetItem,
                            getItemCount,
                            getFilteredKeys,
                            makeReadOnly)

import           ChanStore.Lib.Types hiding (CreateResult(..), UpdateResult(..), CloseResult(..))
import           PayChanServer.Config.Types (ServerDBConf(..))
import           Data.Bitcoin.PaymentChannel.Types
import           Data.Bitcoin.PaymentChannel.Util (unsafeUpdateRecvState)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Monad.Catch (finally, handleAll)
import           Control.Concurrent (forkIO, killThread, threadDelay)
-- import           Control.Concurrent (ThreadId,  threadDelay)


logImportantError = putStrLn


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
destroyChanMap (ChannelMap chanMap Nothing) = return ()

getChanState :: ChannelMap -> Key -> IO (Maybe ChanState)
getChanState (ChannelMap chanMap _) = getItem chanMap

addChanState :: ChannelMap -> Key -> ReceiverPaymentChannel -> IO CreateResult
addChanState (ChannelMap chanMap _) key chanState =
    addItem chanMap key (ReadyForPayment chanState)

updateChanState :: ChannelMap -> Key -> Payment -> IO (MapItemResult Key ChanState)
updateChanState (ChannelMap chanMap _) key payment =
    let
        updateIfOpen (ReadyForPayment rpc) =
            Just $ ReadyForPayment (unsafeUpdateRecvState rpc payment)
        updateIfOpen _ =
            Nothing
    in
        mapGetItem chanMap updateIfOpen key

mapLen (ChannelMap chanMap _) = getItemCount chanMap

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
        ( logImportantError . ("FATAL: Exception while syncing to disk: " ++) . show )
        ( do
              threadDelay delayMicroSecs
              syncNow syncAction
        )
    )
    `finally` (    -- Regardless of what happens, keep thread running
        syncThread syncAction delaySecs )



-- Helper functions
isSettled :: ChanState -> Bool
isSettled (ChannelSettled _ _ _) = True
isSettled _                      = False

isOpen :: ChanState -> Bool
isOpen (ReadyForPayment _) = True
isOpen _                   = False
