module DiskStore
(
DiskMap(..),
newDiskMap, addItem, getItem, updateStoredItem, deleteStoredItem,
mapGetItem, getAllItems,
mapGetItemCount, getFilteredItems,

syncToDisk,mapDiskSyncThread,syncMapToDisk,

Serializable(..),
ToFileName(..),

-- temp
itemContent,

-- re-exports
Hashable(..)
)
where


---old---
import System.Directory (getCurrentDirectory, getDirectoryContents, removeFile, doesFileExist)
import System.IO.Error (isDoesNotExistError)
import qualified ListT as LT
import Data.Hashable
-- import Data.Serialize
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import Control.Monad.STM
import Control.Exception (IOException)
import Control.Monad.Catch (bracket, finally, try)
import Control.Monad (guard, forM, unless, forM_, filterM)
import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import qualified  STMContainers.Map as Map
-- import qualified Data.Binary as Bin
---(old)---

-- import Network.Haskoin.Transaction (TxHash, hexToTxHash)


import qualified Data.ByteString as BS


type STMMap k v = Map.Map k (MapItem v)

data DiskMap k v = DiskMap { mapConf :: MapConfig, diskMap :: STMMap k v }

data MapItem v = Item {
    itemContent :: v
    ,needsDiskSync :: Bool
    ,deleteFromDisk :: Bool }
    deriving Show

data MapConfig = MapConfig {
    syncDir :: FilePath
}

class Serializable a where
    serialize   :: a -> BS.ByteString
    deserialize :: BS.ByteString -> Either String a

class (Serializable k, Eq k, Hashable k) => ToFileName k where
    toFileName      :: k -> String
    fromFileName    :: String -> Either String k

    toFileName = C.unpack . B16.encode . serialize
    fromFileName = deserialize . fst . B16.decode . C.pack


type ItemExists = Bool


--- Interface ---
    -- Root --
newDiskMap :: (ToFileName k, Serializable v) => FilePath -> IO (DiskMap k v)
newDiskMap syncDir = diskGetStateFiles syncDir >>= channelMapFromStateFiles syncDir
    -- Root --

    -- User --
getItem :: ToFileName k =>
    DiskMap k v -> k -> IO (Maybe v)
getItem (DiskMap _ m) k = do
    item <- atomically $ Map.lookup k m
    case item of
        Just Item { deleteFromDisk = True } -> return Nothing
        _ -> return $ fmap itemContent item

addItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> IO ()
addItem dm@(DiskMap _ m) k v =
    atomically (insertItem k v m) >>
    syncNow dm k v

updateStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> IO ItemExists
updateStoredItem dm@(DiskMap _ m) k v = do
    maybeItem <- getItem dm k
    case maybeItem of
        Nothing -> return False
        Just _ ->
            atomically (updateItem m k v) >>
            syncNow dm k v >>
            return True

deleteStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> IO ItemExists
deleteStoredItem dm@(DiskMap _ m) k = do
    maybeItem <- getItem dm k
    case maybeItem of
        Nothing -> return False
        Just v ->
--             atomically (markForDeletion k dm) >>     -- used when disk-sync thread syncs
            performAction dm (k, Delete) >>
            return True

getAllItems :: (ToFileName k, Serializable v) =>
                DiskMap k v -> IO [v]
getAllItems (DiskMap _ m) =
    atomically $ map (itemContent . snd) <$> LT.toList (Map.stream m)

    -- User --
--- Interface ---

mapGetItemCount :: DiskMap k v -> IO Integer
mapGetItemCount (DiskMap _ m) =
    atomically $ fromIntegral . length <$>
        LT.toList (Map.stream m)

getFilteredItems :: DiskMap k v -> (v -> Bool) -> IO [v]
getFilteredItems dm =
    atomically . fmap (map snd) . (getFilteredKV dm)

getFilteredKeys :: DiskMap k v -> (v -> Bool) -> IO [k]
getFilteredKeys dm =
    atomically . fmap (map fst) . (getFilteredKV dm)

getFilteredKV :: DiskMap k v -> (v -> Bool) -> STM [(k,v)]
getFilteredKV (DiskMap _ m) filterBy =
    filter (filterBy . snd) . map (mapSnd itemContent) <$>
        LT.toList (Map.stream m)

data Action = Sync | Delete | Ignore

itemToAction :: MapItem v -> Action
itemToAction Item { deleteFromDisk = True } = Delete
itemToAction Item { needsDiskSync = True } = Sync
itemToAction (Item _ False False) = Ignore


type FileName = FilePath

syncNow :: (ToFileName k, Serializable v) =>
    DiskMap k v
    -> k
    -> v
    -> IO ()
syncNow (DiskMap (MapConfig dir) _) k v = writeEntryToFile dir (k, v)

mapDiskSyncThread ::
    (ToFileName k, Serializable v) =>
    DiskMap k v
    -> Int -- ^Sync interval, in microseconds
    -> IO ()
mapDiskSyncThread m delay =
    (threadDelay delay >> syncToDisk m) `finally` mapDiskSyncThread m delay

syncToDisk ::
    (ToFileName k, Serializable v) =>
    DiskMap k v
    -> IO ()
syncToDisk m = do
    eitherCount <- try $ syncMapToDisk m
    case eitherCount of
        Left  e    -> putStrLn $ "ERROR: Disk sync failed! " ++ show (e :: IOException)
        Right syncCount ->
            unless (syncCount == 0) $
                putStrLn $ "Synced " ++ show syncCount ++ " channel state(s) to disk."


channelMapFromStateFiles ::
    (Serializable v, ToFileName k) =>
    FilePath
    -> [(k,FilePath)]
    -> IO (DiskMap k v)
channelMapFromStateFiles baseDir l = do
    m <- atomically Map.new
    readRes <- forM l tryReadStateFile
    mapRes <- atomically $ forM readRes
        (either error (\(h,s) -> insertDiskSyncedChannel h s m))
    return $ DiskMap (MapConfig baseDir) m

diskGetStateFiles :: ToFileName k => FilePath -> IO [(k,FilePath)]
diskGetStateFiles baseDir = do
    maybeHashPathL <- map (getHashAndFilePath baseDir) <$> getFileList baseDir
    let dirList = [fromJust t | t <- maybeHashPathL, isJust t]
    filterM (doesFileExist . snd) dirList   -- remove directories from list

getFileList :: FilePath -> IO [FilePath]
getFileList dir = do --filter (\f -> f /= "." && f /= "..") <$> getDirectoryContents dir
    r <- try $ getDirectoryContents dir
    case r of
        Left  e    -> fail $ show (e :: IOException) -- "State storage directory \"" ++ dir ++ "\" does not exist."
        Right fileList -> return $ filter (\f -> f /= "." && f /= "..") fileList

tryReadStateFile ::
    (Serializable v, ToFileName k) =>
--     FilePath
    (k, FileName)
    -> IO (Either String (k, v))
tryReadStateFile (h,f) = do
    e <- deserialize <$> B.readFile f
    case e of
        Right val -> return $ Right (h,val)
        Left e    -> return $ Left $ "ERROR: Failed to parse state file: " ++ f

getHashAndFilePath :: ToFileName k => FilePath -> FileName -> Maybe (k,FilePath)
getHashAndFilePath baseDir f  =  mkTuple <$>
    validFileName f <*>
    Just (baseDir ++ "/" ++ f)

validFileName :: ToFileName k => FilePath -> Maybe k
validFileName fn = either (const Nothing) Just (fromFileName fn)

mkTuple :: a -> b -> (a,b)
mkTuple a b = (a,b)
----

singleStateDeleteDisk :: ToFileName k => FilePath -> k -> IO ()
singleStateDeleteDisk stateDir v = removeFile $ getAbsoluteFilePath stateDir v

writeEntryToFile :: (Serializable v, ToFileName k) => String -> (k, v) -> IO ()
writeEntryToFile stateDir (h,s) =
    B.writeFile (getAbsoluteFilePath stateDir h) (serialize s)

getAbsoluteFilePath :: ToFileName k => FilePath -> k -> FilePath
getAbsoluteFilePath stateDir key =
    stateDir ++ toFileName key

------INTERFACE-----


----disk sync
-- | Sync ChannelMap to disk
syncMapToDisk :: (Serializable v, ToFileName k) => DiskMap k v -> IO Int
syncMapToDisk m = do
    keyActions <- getKeyActions m
    forM_ keyActions (performAction m)
    return $ length keyActions

getKeyActions :: DiskMap k v -> IO [(k, Action)]
getKeyActions (DiskMap conf m) = atomically $
        map (mapSnd itemToAction)
        . filter (\(h,i) -> needsDiskSync i || deleteFromDisk i)
        <$> mapAsList m
            where mapAsList = LT.toList . Map.stream

----
performAction :: (Serializable v, ToFileName k) =>  DiskMap k v -> (k, Action) -> IO ()

performAction m (h,Sync)    =
    syncKeyDataToDisk m h

performAction (DiskMap conf m) (h,Delete)  =
    singleStateDeleteDisk (syncDir conf) h >>
    atomically (Map.delete h m)

performAction m (h,Ignore)  = return ()
----


syncKeyDataToDisk :: (Serializable v, ToFileName k) => DiskMap k v -> k -> IO ()
syncKeyDataToDisk dm@(DiskMap conf m) h = do
    maybeItem <- atomically $ getItem_MarkSynced h dm
    maybeSyncItemToDisk (syncDir conf) h maybeItem

-- | Execute item-getter/sync-status-setter and save result to disk
maybeSyncItemToDisk :: (Serializable v, ToFileName k) =>
    FilePath
    -> k
    -> Maybe (MapItem v)
    -> IO ()
maybeSyncItemToDisk dir h maybeItem =
    case maybeItem of
        Just i -> writeEntryToFile dir (h, itemContent i)
        Nothing -> return ()

mapGetItem m f k = mapGetItem_Internal m (\i@Item { itemContent = v } -> i { itemContent = f v } )

-- | If a an item exists, read item i, replace in map with (f i), and return
-- | Maybe (f i).
mapGetItem_Internal :: ToFileName k => DiskMap k v -> (MapItem v -> MapItem v) -> k -> STM (Maybe (MapItem v))
mapGetItem_Internal (DiskMap _ m) f h  = do
    maybeItem <- Map.lookup h m
    case maybeItem of
        Just item ->
            Map.delete h m >> Map.insert (f item) h m >> return (Just $ f item)
        Nothing -> return Nothing

-- | Retrieve map item and simultaneously mark the retrieved item for disk deletion
markForDeletion :: ToFileName k => k -> DiskMap k v -> STM (Maybe (MapItem v))
markForDeletion h m =
    mapGetItem_Internal m
        (\i@Item {} -> i { deleteFromDisk = True }) h

-- | Retrieve map item and simultaneously mark the retrieved item as synced to disk
getItem_MarkSynced:: ToFileName k => k -> DiskMap k v -> STM (Maybe (MapItem v))
getItem_MarkSynced h m =
    mapGetItem_Internal m
        (\i@Item {} -> i { needsDiskSync = False }) h


--- Helper functions ---
updateItem :: (ToFileName k, Serializable v) => STMMap k v -> k -> v -> STM ()
updateItem m k v = do
    Map.delete k m
    insertItem k v m


insertItem :: ToFileName k => k -> v -> STMMap k v -> STM ()
insertItem key item = Map.insert
    Item { itemContent = item, needsDiskSync = True, deleteFromDisk = False } key

insertDiskSyncedChannel :: ToFileName k => k -> v -> STMMap k v -> STM ()
insertDiskSyncedChannel k item = Map.insert
    Item { itemContent = item, needsDiskSync = False, deleteFromDisk = False } k
--- Helper functions ---


------- === UTIL === -------

mapSnd f (a,b)= (a,f b)


------- === UTIL === -------
