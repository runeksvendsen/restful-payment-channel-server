module DiskStore
(
DiskMap(..),
newDiskMap, addItem, getItem, updateStoredItem, -- deleteStoredItem,
mapGetItem,
getAllItems, getItemCount,
getFilteredItems, getFilteredKeys,

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
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import Control.Monad.STM
import Control.Exception (IOException)
import Control.Monad.Catch (bracket, finally, try)
import Control.Monad (guard, forM, unless, forM_, filterM)
import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Spawn (parMapIO)
import qualified  STMContainers.Map as Map



type STMMap k v = Map.Map k (MapItem v)

data DiskMap k v = DiskMap { mapConf :: MapConfig, diskMap :: STMMap k v }

data MapItem v = Item {
    itemContent :: v
    ,needsDiskSync          :: Bool      -- DEPRECATED
    ,isBeingDeletedFromDisk :: Bool }
    deriving Show

data MapConfig = MapConfig {
    syncDir :: FilePath
}


data MapUpdateResult k v =
    Updated k v |
    NotUpdated |
    DoesntExist

class Serializable a where
    serialize   :: a -> BS.ByteString
    deserialize :: BS.ByteString -> Either String a

class (Serializable k, Eq k, Hashable k) => ToFileName k where
    toFileName      :: k -> String
    fromFileName    :: String -> Either String k

    toFileName = C.unpack . B16.encode . serialize
    fromFileName = deserialize . fst . B16.decode . C.pack


type ItemExists = Bool

getSyncDir = syncDir . mapConf


--- Interface ---
    -- Root --
newDiskMap :: (ToFileName k, Serializable v) => FilePath -> IO (DiskMap k v)
newDiskMap syncDir = diskGetStateFiles syncDir >>= channelMapFromStateFiles syncDir
    -- Root --

    -- User --
getItem :: ToFileName k =>
    DiskMap k v -> k -> STM (Maybe v)
getItem (DiskMap _ m) k =
    fmap itemContent <$> fetchItem m k

addItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> IO ()
addItem dm@(DiskMap _ m) k v = do
    _updateMapItem (getSyncDir dm) $
        insertItem k v m >> return (Updated k v)
    return ()

updateStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> IO ItemExists
updateStoredItem m k v =
    mapGetItem m (const . Just $ v) k >>= transformReturnVal
        where transformReturnVal res =
                case res of
                    (Updated _ _) -> return True
                    _             -> return False

mapStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> (v -> Maybe v) -> k -> STM (MapUpdateResult k v)
mapStoredItem dm@(DiskMap _ m) f k = do
    maybeItem <- getItem dm k
    case maybeItem of
        Nothing -> return DoesntExist
        Just val -> maybeUpdate
            where maybeUpdate =
                    case f val of
                        Just newVal ->
                            updateItem m k newVal >>
                            return (Updated k newVal)
                        Nothing ->
                            return NotUpdated

mapGetItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> (v -> Maybe v) -> k -> IO (MapUpdateResult k v)
mapGetItem dm f k = _updateMapItem (getSyncDir dm) $ mapStoredItem dm f k

getAllItems :: (ToFileName k, Serializable v) => DiskMap k v -> IO [v]
getAllItems (DiskMap _ m) =
    atomically $ map (itemContent . snd) <$> LT.toList (Map.stream m)

    -- User --
--- Interface ---

getItemCount :: DiskMap k v -> IO Integer
getItemCount (DiskMap _ m) =
    atomically $ fromIntegral . length <$>
        LT.toList (Map.stream m)

getFilteredItems :: DiskMap k v -> (v -> Bool) -> STM [v]
getFilteredItems dm =
    fmap (map snd) . (getFilteredKV dm)

getFilteredKeys :: DiskMap k v -> (v -> Bool) -> STM [k]
getFilteredKeys dm =
    fmap (map fst) . (getFilteredKV dm)

getFilteredKV :: DiskMap k v -> (v -> Bool) -> STM [(k,v)]
getFilteredKV (DiskMap _ m) filterBy =
    filter (filterBy . snd) . map (mapSnd itemContent) <$>
        LT.toList (Map.stream m)
            where mapSnd f (a,b)= (a,f b)


-- Disk sync -- {

-- |Once the map has been initialized, this should be the only function that updates item contents
_updateMapItem :: (ToFileName k, Serializable v) =>
    String -> STM (MapUpdateResult k v) -> IO (MapUpdateResult k v)
_updateMapItem dir updateAction = do
    updateRes <- atomically $ do updateAction
    case updateRes of
        Updated key val -> writeEntryToFile dir (key,val)
        _ -> return ()
    return updateRes

_deleteMapItem :: (ToFileName k) =>
    DiskMap k v -> k -> IO Bool
_deleteMapItem dm@(DiskMap _ m) k = do
    exists <- fmap isJust $ atomically $ markAsBeingDeleted dm k
    if not exists then
            return False
        else do
            singleStateDeleteDisk (getSyncDir dm) k
            atomically $ Map.delete k m
            return True

data Action = Sync | Delete | Ignore
--
-- itemToAction :: MapItem v -> Action
-- itemToAction Item { deleteFromDisk = True } = Delete
-- itemToAction Item { needsDiskSync = True } = Sync
-- itemToAction (Item _ False False) = Ignore

type FileName = FilePath

channelMapFromStateFiles ::
    (Serializable v, ToFileName k) =>
    FilePath
    -> [(k,FilePath)]
    -> IO (DiskMap k v)
channelMapFromStateFiles baseDir l = do
    m <- atomically Map.new
    readRes <- parMapIO tryReadStateFile l
    mapRes <- atomically $ forM readRes
        (either error (\(h,s) -> insertDiskSyncedChannel h s m))
    return $ DiskMap (MapConfig baseDir) m

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
        Left e    -> return $ Left $ "ERROR: Failed to parse state file: " ++ f

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

writeEntryToFile :: (Serializable v, ToFileName k) => String -> (k, v) -> IO ()
writeEntryToFile stateDir (h,s) =
    BS.writeFile (mkAbsoluteFilePath stateDir h) (serialize s)

-- Disk sync -- }


-- | If a an item exists, read item i, replace in map with (f i), and return
-- | Maybe i. Note that the original, unmapped, item is returned.
mapGetItem_Internal :: ToFileName k =>
    DiskMap k v
    -> (MapItem v -> MapItem v)
    -> k
    -> STM (Maybe (MapItem v))
mapGetItem_Internal (DiskMap _ m) f k  = do
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

updateItem :: (ToFileName k, Serializable v) => STMMap k v -> k -> v -> STM Bool
updateItem m k v = do
    maybeItem <- fetchItem m k
    case maybeItem of
        Just i -> insertItem k v m >> return True
        Nothing -> return False

insertItem :: ToFileName k => k -> v -> STMMap k v -> STM ()
insertItem key item = Map.insert
    Item { itemContent = item, needsDiskSync = True, isBeingDeletedFromDisk = False } key

insertDiskSyncedChannel :: ToFileName k => k -> v -> STMMap k v -> STM ()
insertDiskSyncedChannel k item = Map.insert
    Item { itemContent = item, needsDiskSync = False, isBeingDeletedFromDisk = False } k
--- Helper functions ---
