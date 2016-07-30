module Data.DiskMap.Internal.Update where

import Data.DiskMap.Types
import Data.DiskMap.Internal.Helpers (mapGetItem_Internal)
import Data.DiskMap.Sync.Sync (writeEntryToFile)
import Data.DiskMap.Sync.Deferred (runSyncAction)

import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad.STM
import Control.Monad (forM, filterM, when)
import qualified  STMContainers.Map as Map
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception.Base     (Exception, throwIO)


-- | Retrieve map item and simultaneously mark the retrieved item for disk deletion
markAsBeingDeleted :: ToFileName k => DiskMap k v -> k -> STM (Maybe (MapItem v))
markAsBeingDeleted m k =
    mapGetItem_Internal m
        (\i@Item {} -> i { isBeingDeletedFromDisk = True }) k


guardReadOnly :: DiskMap k v -> IO (DiskMap k v)
guardReadOnly dm@(DiskMap (MapConfig _ _) _ _ readOnlyTVar) = do
    readOnly <- atomically $ readTVar readOnlyTVar
    if not readOnly then do
            return dm
        else
            throwIO PermissionDenied

updateMapItem dm action =
    guardReadOnly dm >>=
        flip _updateMapItem action

-- |Once the map has been initialized, this should be the only function that updates item contents
_updateMapItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> STM [MapItemResult k v] -> IO [MapItemResult k v]
_updateMapItem (DiskMap (MapConfig dir deferSync) _ (SyncState deferredSyncMap _) readOnlyTVar) updateActions = do
    readOnly <- atomically $ readTVar readOnlyTVar
    if not readOnly then do
            updateResults <- atomically $ do updateActions
            let syncToDisk res =
                    case res of
                        ItemUpdated key val ->
                                if not deferSync then   -- Write to disk immediately
                                    writeEntryToFile dir key val
                                else   -- Note key, as well as the fact that it lacks sync
                                    atomically $ Map.insert Sync key deferredSyncMap
                        _ -> return ()
            forM updateResults syncToDisk
            return updateResults
        else
            throwIO PermissionDenied

-- |Not finished yet
_deleteMapItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> IO Bool
_deleteMapItem dm@(DiskMap (MapConfig syncDir deferSync) m (SyncState deferredSyncMap _) _) key = do
    exists <- fmap isJust $ atomically $ markAsBeingDeleted dm key
    if not exists then
            return False
        else do
            if not deferSync then
                    runSyncAction syncDir m (key,Delete) >> return ()
                else
                    atomically $ Map.insert Delete key deferredSyncMap
            return True
