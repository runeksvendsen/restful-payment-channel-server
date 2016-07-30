module Data.DiskMap.Sync.Deferred where

import Data.DiskMap.Types
import Data.DiskMap.Sync.Sync (writeEntryToFile, singleStateDeleteDisk)

import qualified ListT as LT
import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad.STM
import Control.Monad (forM, filterM, when)
import qualified  STMContainers.Map as Map
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, tryTakeMVar)



-- |Prevent multiple sync operations from running simultaneously
startSyncOrWait :: (ToFileName k, Serializable v) => DiskMap k v -> IO (DiskMap k v)
startSyncOrWait dm@(DiskMap _ _ (SyncState _ syncInProgress) _) = do
    maybeVal <- tryTakeMVar syncInProgress
    when (isNothing maybeVal) $
        putStrLn $ "WARNING: Disk sync is waiting because another one is running already." ++
            " Disk fast enough?"
    takeMVar syncInProgress
    putStrLn $ "DiskMap sync: TOOK mvar"
    return dm

setSyncDone :: (ToFileName k, Serializable v) =>
     (DiskMap k v, Integer) -> IO Integer
setSyncDone ( (DiskMap _ _ (SyncState _ syncInProgress) _) , syncCount ) =
    putMVar syncInProgress () >>
    putStrLn ("DiskMap sync: PUT mvar") >>
    return syncCount

syncToDiskNow dm = startSyncOrWait dm >>= _syncToDiskNow >>= setSyncDone

_syncToDiskNow :: (ToFileName k, Serializable v) =>
     DiskMap k v -> IO (DiskMap k v,Integer)
_syncToDiskNow dm@(DiskMap (MapConfig syncDir True) diskMap (SyncState deferredSyncMap _) _) = do
    -- Get all items from syncMap and simultaneously clear it
    syncKeyActionList <- atomically $ do
        vals <- LT.toList (Map.stream deferredSyncMap)
        -- Delete all items
        forM vals (\(key,_) -> Map.delete key deferredSyncMap)
        return vals
    -- For each key in the syncMap, lookup>>=write or delete>>delete in diskMap
    successList <- forM syncKeyActionList (runSyncAction syncDir diskMap)
    if all (== True) successList then
            return $ (dm, fromIntegral $ length successList)
        else
            error $ "BUG: Key-to-be-synced not present in DiskMap"
_syncToDiskNow (DiskMap (MapConfig _ False) _ _ _) =
    error "BUG: How did you get a sync IO action if delaySync is disabled?"

--  Don't try to be a hero: get/delete only one item at a time from the diskMap.
--  This is most relevant when the map is under heavy use, where an operation
--  that fetches all items from the map simultaneously might be prevented from
--  finishing.
runSyncAction :: (ToFileName k, Serializable v) =>
    FilePath
    -> STMMap k v
    -> (k,SyncOp)
    -> IO Success
runSyncAction dir m (key, Sync) = do
    maybeVal <- atomically (Map.lookup key m)
    case maybeVal of
        Just val -> writeEntryToFile dir key (itemContent val) >> return True
        Nothing  -> return False
runSyncAction dir m (key, Delete)=
    atomically (Map.delete key m) >>
    singleStateDeleteDisk dir key >>
    return True
