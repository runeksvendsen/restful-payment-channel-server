module DiskStore
(
DiskMap,
newDiskMap, addItem, getItem, updateStoredItem, deleteStoredItem,
mapGetItemCount,getFilteredItems,

syncToDisk,mapDiskSyncThread,syncMapToDisk,

Serializable(..),
ToFileName(..),

-- re-exports
Hashable(..)
)
where


---old---
import System.Directory (getCurrentDirectory, getDirectoryContents, removeFile)
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
import Control.Monad (guard, forM, unless, forM_)
import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import qualified  STMContainers.Map as Map
-- import qualified Data.Binary as Bin
---(old)---

-- import Network.Haskoin.Transaction (TxHash, hexToTxHash)

import qualified STMContainers.Map as STMap
import qualified Data.ByteString as BS


type STMMap k v = Map.Map k (MapItem v)

data DiskMap k v = DiskMap MapConfig (STMMap k v)

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
addItem :: ToFileName k =>
    DiskMap k v -> k -> v -> STM ()
addItem (DiskMap _ m) h pc = insertItem h pc m

getItem :: ToFileName k =>
    DiskMap k v -> k -> STM (Maybe v)
getItem (DiskMap _ m) k = do
    item <- Map.lookup k m
    case item of
        Just Item { deleteFromDisk = True } -> return Nothing
        _ -> return $ fmap itemContent item

updateStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> STM ItemExists
updateStoredItem dm@(DiskMap _ m) k v = do
    maybeItem <- getItem dm k
    case maybeItem of
        Nothing -> return False
        Just _ ->  updateItem m k v >> return True

deleteStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> STM ItemExists
deleteStoredItem dm@(DiskMap _ m) k = do
    maybeItem <- getItem dm k
    case maybeItem of
        Nothing -> return False
        Just _ ->  markForDeletion k dm >> return True
    -- User --
--- Interface ---

mapGetItemCount :: DiskMap k v -> IO Integer
mapGetItemCount (DiskMap _ m) =
    atomically $ fromIntegral . length <$>
        LT.toList (Map.stream m)

getFilteredItems :: DiskMap k v -> (v -> Bool) -> STM [v]
getFilteredItems (DiskMap _ m) filterBy =
    filter filterBy . map (itemContent . snd) <$>
        LT.toList (Map.stream m)


data Action = Sync | Delete | Ignore

itemToAction :: MapItem v -> Action
itemToAction Item { deleteFromDisk = True } = Delete
itemToAction Item { needsDiskSync = True } = Sync
itemToAction (Item _ False False) = Ignore


type FileName = FilePath


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
    return [fromJust t | t <- maybeHashPathL, isJust t]

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
maybeSyncItemToDisk ::
    (Serializable v, ToFileName k) =>
    FilePath
    -> k
    -> Maybe (MapItem v) -> IO ()
maybeSyncItemToDisk dir h maybeItem =
    case maybeItem of
        Just i -> writeEntryToFile dir (h, itemContent i)  --singleStateSyncDisk (h, itemContent i)
        Nothing -> return ()

-- | Retrieve map item and simultaneously mark the retrieved item as synced to disk
getItem_MarkSynced:: ToFileName k => k -> DiskMap k v -> STM (Maybe (MapItem v))
getItem_MarkSynced h =
    mapAndGetItem h
        (\i@Item {} -> i { needsDiskSync = False })

-- | If a an item exists, read item i, replace in map with (f i), and return
-- | Maybe (f i).
mapAndGetItem :: ToFileName k => k -> (MapItem v -> MapItem v) -> DiskMap k v -> STM (Maybe (MapItem v))
mapAndGetItem h f (DiskMap _ m) = do
    maybeItem <- Map.lookup h m
    case maybeItem of
        Just item ->
            Map.delete h m >> Map.insert (f item) h m >> return (Just $ f item)
        Nothing -> return Nothing

-- | Retrieve map item and simultaneously mark the retrieved item for disk deletion
markForDeletion :: ToFileName k => k -> DiskMap k v -> STM (Maybe (MapItem v))
markForDeletion h =
    mapAndGetItem h
        (\i@Item {} -> i { deleteFromDisk = True })


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
