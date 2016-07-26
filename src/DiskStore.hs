module DiskStore
(
DiskMap(..),
newDiskMap, addItem, getItem, updateStoredItem,
CreateResult(..),
mapGetItem, mapGetItems, MapItemResult(..),
getAllItems, getItemCount,
getFilteredItems, getFilteredKeys, getFilteredKV,

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

data CreateResult = Created | AlreadyExists


data MapItemResult k v =
    ItemUpdated k v |
    NotUpdated |
    NoSuchItem

class Serializable a where
    serialize   :: a -> BS.ByteString
    deserialize :: BS.ByteString -> Either String a

class (Serializable k, Eq k, Hashable k) => ToFileName k where
    toFileName      :: k -> String
    fromFileName    :: String -> Either String k

    toFileName = C.unpack . B16.encode . serialize
    fromFileName = deserialize . fst . B16.decode . C.pack


type ItemExists = Bool
-- |Represents a map update action, affecting value "v" at key "k"
data ComposableUpdate k v = ComposableUpdate (STM [MapItemResult k v])

getSyncDir = syncDir . mapConf


--- Interface ---
    -- Root --
newDiskMap :: (ToFileName k, Serializable v) => FilePath -> IO (DiskMap k v)
newDiskMap syncDir = diskGetStateFiles syncDir >>= channelMapFromStateFiles syncDir
    -- Root --

    -- User --
getItem :: ToFileName k =>
    DiskMap k v -> k -> IO (Maybe v)
getItem m = atomically . getItem' m

getItem' :: ToFileName k =>
    DiskMap k v -> k -> STM (Maybe v)
getItem' (DiskMap _ m) k =
    fmap itemContent <$> fetchItem m k

addItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> IO CreateResult
addItem dm@(DiskMap _ m) k v = do
    res <- fmap head $ _updateMapItem (getSyncDir dm) $
        getItem' dm k >>= maybe
            (insertItem k v m >> return [ItemUpdated k v])  -- Doesn't already exist
            (const $ return [NotUpdated])                 -- Already exists
    case res of
        ItemUpdated _ _ -> return Created
        NotUpdated  -> return AlreadyExists
        _           -> error "BUG 20:56:49"

updateStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> k -> v -> IO (MapItemResult k v)
updateStoredItem m k v =
    mapGetItem m (const . Just $ v) k

mapGetItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> (v -> Maybe v) -> k -> IO (MapItemResult k v)
mapGetItem dm f k = head <$> mapGetItems dm f ( return [k] )

mapGetItems :: (ToFileName k, Serializable v) =>
    DiskMap k v -> (v -> Maybe v) -> STM [k] -> IO [MapItemResult k v]
mapGetItems dm f stmKeys = _updateMapItem (getSyncDir dm) $ do
    keys <- stmKeys
    forM keys (mapStoredItem dm f)

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


-- |If 'f' applied to the value at 'k' in the map returns a Just, then update the value
--  to this. Return information about what happened.
mapStoredItem :: (ToFileName k, Serializable v) =>
    DiskMap k v -> (v -> Maybe v) -> k -> STM (MapItemResult k v)
mapStoredItem dm@(DiskMap _ m) f k = do
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

-- mkComposableUpdate :: (ToFileName k, Serializable v) =>
--     DiskMap k v -> (v -> Maybe v) -> k -> ComposableUpdate k v
-- mkComposableUpdate dm f k = ComposableUpdate dm $ mapStoredItem dm f k
--
-- atomicallyDoUpdates :: (ToFileName k, Serializable v) =>
--     [ComposableUpdate k v] -> IO [MapItemResult k v]
-- atomicallyDoUpdates actions = _updateMapItem (getSyncDir dm)


-- Disk sync -- {

-- |Once the map has been initialized, this should be the only function that updates item contents
_updateMapItem :: (ToFileName k, Serializable v) =>
    String -> STM [MapItemResult k v] -> IO [MapItemResult k v]
_updateMapItem dir updateActions = do
    updateResults <- atomically $ do updateActions
    let syncToDisk res =
            case res of
                ItemUpdated key val -> writeEntryToFile dir (key,val)
                _ -> return ()
    forM updateResults syncToDisk
    return updateResults

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

-- data Action = Sync | Delete | Ignore
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
channelMapFromStateFiles baseDir l =
    let
        forSequential = forM
        -- It may be problematic to open all files in parallel (due to max open files limit),
        --  so this is turned off  by default.
        forParallel = flip parMapIO
    in do
        m <- atomically Map.new
        readRes <- forSequential l tryReadStateFile
        atomically $ forM readRes
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
