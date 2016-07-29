{-|
Module      : DiskMap
Description : Persistent STM Map
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

An STM 'STMContainers.Map' which syncs each map operation to disk,
 from which the map can be restored later. Each key is stored as a
 file with the content being the serialized map item. So this is optimized
 for access to relatively large state objects, where storing a file on disk for
 each item in the map is not an issue.
Optionally, writing to disk can be deferred, such that each map update
 doesn't touch the disk immediately, but instead only when the 'DiskSync'
 IO action, returned by 'newDiskMap', is evaluated.
This database is ACID-compliant, although the durability property is lost
 if deferred disk writes are enabled.
-}

module DiskMap
(
DiskMap,
newDiskMap, addItem, getItem, updateStoredItem,
CreateResult(..),
mapGetItem, mapGetItems, MapItemResult(..),
getAllItems, getItemCount,
getFilteredItems, getFilteredKeys, getFilteredKV,
SyncAction,isSynced,makeReadOnly,
Serializable(..),
ToFileName(..),

-- re-export
Hashable(..)
)
where


import System.Directory (getDirectoryContents, removeFile, doesFileExist)
import qualified ListT as LT
import Data.Hashable
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import Control.Monad.STM
import Control.Exception (IOException)
import Control.Monad.Catch (try)
import Control.Monad (forM, filterM, when)
import Control.Concurrent.Spawn (parMapIO)
import qualified  STMContainers.Map as Map
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import Control.Exception.Base     (Exception, throwIO)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, tryTakeMVar)


-- |
data DiskMap k v = DiskMap
    MapConfig
    (STMMap k v)
    (SyncState k)
    (TVar Bool)     -- Read-only?

type STMMap k v = Map.Map k (MapItem v)

data MapItem v = Item {
    itemContent :: v
    ,needsDiskSync          :: Bool      -- DEPRECATED
    ,isBeingDeletedFromDisk :: Bool }
    deriving Show

data MapConfig = MapConfig FilePath Bool

data SyncState k = SyncState
    (Map.Map k SyncOp)  -- Deferred sync map
    (MVar ())           -- If empty: sync is in progress

-- |Disk sync IO action. Returns the number of disk items that were
--  either updated or deleted.
type SyncAction = IO Integer

data CreateResult = Created | AlreadyExists
data WriteException = PermissionDenied deriving (Show, Eq)
instance Exception WriteException

-- |
data MapItemResult k v =
    ItemUpdated k v |
    NotUpdated |
    NoSuchItem

-- | Types that can be serialized and deserialized
class Serializable a where
    serialize   :: a -> BS.ByteString
    deserialize :: BS.ByteString -> Either String a

-- | Types that can be converted to a unique filename
class (Serializable k, Eq k, Hashable k) => ToFileName k where
    toFileName      :: k -> String
    fromFileName    :: String -> Either String k

    toFileName = C.unpack . B16.encode . serialize
    fromFileName = deserialize . fst . B16.decode . C.pack


--- Interface ---
-- |
newDiskMap :: (ToFileName k, Serializable v) =>
    FilePath -- ^ Directory where state files will be kept. The map is restored from this directory as well.
    -> Bool  -- ^ Don't sync to disk immediately on each map update, but instead return a sync IO action
             -- which, when evaluated, performs the sync.
             -- Any updates written to the map after evaluating the sync action are lost, unless the
             --  sync action is executed again, before destroying the map. The 'isSynced' function
             --  returns a boolean indicating whether the map in question is synced to disk, or contains
             --  unsyned updates.
    -> IO (DiskMap k v, Maybe SyncAction)  -- ^New map and, optionally, a sync IO action
newDiskMap syncDir deferSync = do
    -- Restore STMMap from disk files
    m <- channelMapFromStateFiles =<<
            diskGetStateFiles syncDir
    -- read-only TVar
    readOnly <- newTVarIO False
    -- Deferred sync map+busyVar
    deferredSyncMap <- Map.newIO
    syncInProgress <- newMVar ()
    let syncState = SyncState deferredSyncMap syncInProgress
    let diskMap = DiskMap (MapConfig syncDir deferSync) m syncState readOnly
    -- Sync IO action
    let maybeSyncFunc =
            if deferSync then
                Just $ syncToDiskNow diskMap
            else
                Nothing
    return (diskMap, maybeSyncFunc)


-- |
getItem :: ToFileName k =>
    DiskMap k v -> k -> IO (Maybe v)
getItem m = atomically . getItem' m


-- |
addItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> IO CreateResult
addItem dm@(DiskMap _ m _ _) k v = do
    res <- fmap head $ updateMapItem dm $
        getItem' dm k >>= maybe
            (insertItem k v m >> return [ItemUpdated k v])  -- Doesn't already exist
            (const $ return [NotUpdated])                 -- Already exists
    case res of
        ItemUpdated _ _ -> return Created
        NotUpdated  -> return AlreadyExists
        _           -> error "BUG 20:56:49"

-- |
updateStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> IO (MapItemResult k v)
updateStoredItem m k v =
    mapGetItem m (const . Just $ v) k

-- |
mapGetItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> (v -> Maybe v) -> k -> IO (MapItemResult k v)
mapGetItem dm f k = head <$> mapGetItems dm f ( return [k] )

-- |
mapGetItems :: (ToFileName k, Serializable v) =>
    DiskMap k v -> (v -> Maybe v) -> STM [k] -> IO [MapItemResult k v]
mapGetItems dm@(DiskMap _ _ _ _) f stmKeys = updateMapItem dm $ do
    keys <- stmKeys
    forM keys (mapStoredItem dm f)

-- |
getAllItems :: (ToFileName k, Serializable v) => DiskMap k v -> IO [v]
getAllItems (DiskMap _ m _ _) =
    atomically $ map (itemContent . snd) <$> LT.toList (Map.stream m)

    -- User --
--- Interface ---

-- |
getItemCount :: DiskMap k v -> IO Integer
getItemCount (DiskMap _ m _ _) =
    atomically $ fromIntegral . length <$>
        LT.toList (Map.stream m)

-- |
getFilteredItems :: DiskMap k v -> (v -> Bool) -> STM [v]
getFilteredItems dm =
    fmap (map snd) . (getFilteredKV dm)

-- |
getFilteredKeys :: DiskMap k v -> (v -> Bool) -> STM [k]
getFilteredKeys dm =
    fmap (map fst) . (getFilteredKV dm)

-- |
getFilteredKV :: DiskMap k v -> (v -> Bool) -> STM [(k,v)]
getFilteredKV (DiskMap _ m _ _) filterBy =
    filter (filterBy . snd) . map (mapSnd itemContent) <$>
        LT.toList (Map.stream m)
            where mapSnd f (a,b)= (a,f b)

-- |Prevent further writes to the map. All write operations will throw an exception after
--  evaluating this function. Only the 'SyncNow' action may alter the map hereafter,
--  by deleting items from the map that were queued for deletion before enabling
--  read-only access.
makeReadOnly :: DiskMap k v -> IO ()
makeReadOnly (DiskMap _ _ _ readOnlyTVar) =
    atomically $ writeTVar readOnlyTVar True

-- |If 'f' applied to the value at 'k' in the map returns a Just, then update the value
--  to this. Return information about what happened.
mapStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> (v -> Maybe v) -> k -> STM (MapItemResult k v)
mapStoredItem dm@(DiskMap _ m _ _) f k = do
    maybeItem <- getItem' dm k
    case maybeItem of
        Nothing  -> return NoSuchItem
        Just val -> maybeUpdate
            where maybeUpdate =
                    case f val of
                        Just newVal ->
                            updateItem m k newVal >>
                            return (ItemUpdated k newVal)
                        Nothing ->
                            return NotUpdated


---- Disk sync ---- {

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

guardReadOnly :: DiskMap k v -> IO (DiskMap k v)
guardReadOnly dm@(DiskMap (MapConfig _ _) _ _ readOnlyTVar) = do
    readOnly <- atomically $ readTVar readOnlyTVar
    if not readOnly then do
            return dm
        else
            throwIO PermissionDenied

-- |
isSynced :: DiskMap k v -> IO Bool
isSynced (DiskMap (MapConfig _ True) _ (SyncState deferredSyncMap _) _) =
    atomically $ (== 0) . length <$> LT.toList (Map.stream deferredSyncMap)
-- If deferred sync is disabled, the map is always in sync with disk
isSynced (DiskMap (MapConfig _ False) _ _ _) = return True

-- |Prevent multiple sync operations from running simultaneously
startSyncOrWait :: (ToFileName k, Serializable v) => DiskMap k v -> IO (DiskMap k v)
startSyncOrWait dm@(DiskMap _ _ (SyncState _ syncInProgress) _) = do
    maybeVal <- tryTakeMVar syncInProgress
    when (isNothing maybeVal) $
        putStrLn $ "WARNING: Disk sync is waiting because another one is running already." ++
            " Disk fast enough?"
    takeMVar syncInProgress
    return dm

setSyncDone :: (ToFileName k, Serializable v) =>
     (DiskMap k v, Integer) -> IO Integer
setSyncDone ( (DiskMap _ _ (SyncState _ syncInProgress) _) , syncCount ) =
    putMVar syncInProgress () >>
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

data SyncOp = Sync | Delete
type Success = Bool

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


type FileName = FilePath

channelMapFromStateFiles ::
    (Serializable v, ToFileName k) =>
    [(k,FilePath)]
    -> IO (STMMap k v) --(DiskMap k v, Maybe SyncAction)
channelMapFromStateFiles keyFilePathList =
    let
        sequentiallyForEach = forM
        -- It may be problematic, depending on map size,
        --  to open all state files at the same time,
        --  due to max concurrent open files limit,
        --  so this is turned off  by default.
        inParallelForEach = flip parMapIO
    in do
        -- Restore map
        m <- atomically Map.new
        readRes <- sequentiallyForEach keyFilePathList tryReadStateFile
        atomically $ forM readRes
            (either error (\(h,s) -> insertDiskSyncedChannel h s m))

        return m

diskGetStateFiles :: ToFileName k => FilePath -> IO [(k,FilePath)]
diskGetStateFiles baseDir = do
    maybeHashPathL <- map (getHashAndFilePath baseDir) <$> getFileList baseDir
    let dirList = [fromJust t | t <- maybeHashPathL, isJust t]
    filterM (doesFileExist . snd) dirList   -- remove directories from list

getFileList :: FilePath -> IO [FilePath]
getFileList dir = do
    r <- try $ getDirectoryContents dir
    case r of
        Left  e    -> fail $ show (e :: IOException) -- "State storage directory \"" ++ dir ++ "\" does not exist."
        Right fileList -> return $ filter (\f -> f /= "." && f /= "..") fileList

tryReadStateFile ::
    (Serializable v, ToFileName k) =>
    (k, FileName)
    -> IO (Either String (k, v))
tryReadStateFile (h,f) = do
    e <- deserialize <$> BS.readFile f
    case e of
        Right val -> return $ Right (h,val)
        Left e    -> return $ Left $
            "ERROR: Failed to parse state file " ++ show f ++ ": " ++ e

getHashAndFilePath :: ToFileName k => FilePath -> FileName -> Maybe (k,FilePath)
getHashAndFilePath baseDir f  = mkTuple <$>
    validFileName f <*>
    Just (baseDir ++ "/" ++ f)
        where mkTuple a b = (a,b)


mkAbsoluteFilePath :: ToFileName k => FilePath -> k -> FilePath
mkAbsoluteFilePath stateDir key =
    stateDir ++ toFileName key

validFileName :: ToFileName k => FilePath -> Maybe k
validFileName fn = either (const Nothing) Just (fromFileName fn)
----

singleStateDeleteDisk :: ToFileName k => FilePath -> k -> IO ()
singleStateDeleteDisk stateDir v = removeFile $ mkAbsoluteFilePath stateDir v

writeEntryToFile :: (Serializable v, ToFileName k) => String -> k -> v -> IO ()
writeEntryToFile stateDir k v =
    BS.writeFile (mkAbsoluteFilePath stateDir k) (serialize v)

-- Disk sync -- }


-- | If a an item exists, read item i, replace in map with (f i), and return
-- | Maybe i. Note that the original, unmapped, item is returned.
mapGetItem_Internal :: ToFileName k =>
    DiskMap k v
    -> (MapItem v -> MapItem v)
    -> k
    -> STM (Maybe (MapItem v))
mapGetItem_Internal (DiskMap _ m _ _) f k  = do
    maybeItem <- fetchItem m k
    case maybeItem of
        (Just item) ->
            Map.insert (f item) k m >> return maybeItem
        Nothing -> return Nothing

-- | Retrieve map item and simultaneously mark the retrieved item for disk deletion
markAsBeingDeleted :: ToFileName k => DiskMap k v -> k -> STM (Maybe (MapItem v))
markAsBeingDeleted m k =
    mapGetItem_Internal m
        (\i@Item {} -> i { isBeingDeletedFromDisk = True }) k

--- Helper functions ---
fetchItem :: ToFileName k =>
    STMMap k v -> k -> STM (Maybe (MapItem v))
fetchItem m k = do
    item <- Map.lookup k m
    case item of
        Just Item { isBeingDeletedFromDisk = True } -> return Nothing
        _ -> return item

getItem' :: ToFileName k =>
    DiskMap k v -> k -> STM (Maybe v)
getItem' (DiskMap _ m _ _) k =
    fmap itemContent <$> fetchItem m k

updateItem :: (ToFileName k, Serializable v) => STMMap k v -> k -> v -> STM Bool
updateItem m k v = do
    maybeItem <- fetchItem m k
    case maybeItem of
        Just _ -> insertItem k v m >> return True
        Nothing -> return False

insertItem :: ToFileName k => k -> v -> STMMap k v -> STM ()
insertItem key item = Map.insert
    Item { itemContent = item, needsDiskSync = True, isBeingDeletedFromDisk = False } key

insertDiskSyncedChannel :: ToFileName k => k -> v -> STMMap k v -> STM ()
insertDiskSyncedChannel k item = Map.insert
    Item { itemContent = item, needsDiskSync = False, isBeingDeletedFromDisk = False } k
--- Helper functions ---
