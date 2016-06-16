module DiskStore where

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
import Control.Exception
import Control.Monad (guard, forM, unless, forM_)
import Control.Arrow (second)
import Control.Concurrent (threadDelay)
import qualified  STMContainers.Map as Map
-- import qualified Data.Binary as Bin
---(old)---

import Network.Haskoin.Transaction (TxHash, hexToTxHash)

import qualified STMContainers.Map as STMap
import qualified Data.ByteString as BS


data Store k v = STMap k (ValWrap v)

data ValWrap v = ValWrap v Flags

data Flags =
    DeleteItem |
    ItemSynced Bool

class Serializable a where
    serialize   :: a -> BS.ByteString
    deserialize :: BS.ByteString -> Either String a

class (Serializable k, Eq k, Hashable k) => ToFileName k where
    toFileName      :: k -> String
    fromFileName    :: String -> Either String k

    toFileName = C.unpack . B16.encode . serialize
    fromFileName = deserialize . fst . B16.decode . C.pack

-- instance Serializable k => Hashable k where
--     hashWithSalt salt pk = hashWithSalt salt (txHashToHex pk)


type ItemExists = Bool
--
-- storeNewItem        :: (Serializable k, v) => Store k v -> k -> v -> IO () --will overwrite
-- updateStoredItem    :: (Serializable k, v) => Store k v -> k -> v -> IO ItemExists
-- lookupItem          :: (Serializable k, v) => Store k v -> k -> IO (Maybe v)
-- removeItem          :: (Serializable k, v) => Store k v -> k -> IO ItemExists
--
-- storeNewItem = undefined
-- updateStoredItem = undefined
-- lookupItem = undefined
-- removeItem = undefined

newDiskMap :: (ToFileName k, Serializable v) => IO (DiskMap k v)
newDiskMap = atomically Map.new

addItem :: ToFileName k => DiskMap k v -> k -> v -> IO ()
addItem m h pc = atomically $ insertItem h pc m

getItem :: ToFileName k => DiskMap k v -> k -> IO (Maybe v)
getItem m k = do
    item <- atomically $ Map.lookup k m
    case item of
        Just Item { deleteFromDisk = True } -> return Nothing
        _ -> return $ fmap itemContent item

updateStoredItem :: (ToFileName k, Serializable v) => DiskMap k v -> k -> v -> IO ItemExists
updateStoredItem m k v = do
    maybeItem <- getItem m k
    case maybeItem of
        Nothing -> return False
        Just _ ->  updateItem m k v >> return True

deleteStoredItem :: (ToFileName k, Serializable v) => DiskMap k v -> k -> IO ItemExists
deleteStoredItem m k = do
    maybeItem <- getItem m k
    case maybeItem of
        Nothing -> markForDeletion k m >> return False
        Just _ ->  markForDeletion k m >> return True

-- ------======000000======-------- --





mapGetItemCount :: DiskMap k v -> IO Integer
mapGetItemCount m = atomically $ fmap (fromIntegral . length) (LT.toList . Map.stream $ m)




type DiskMap k v = Map.Map k (MapItem v)

data MapItem v = Item {
    itemContent :: v
    ,needsDiskSync :: Bool
    ,deleteFromDisk :: Bool }
    deriving Show

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
    syncCount <- syncMapToDisk m
    unless (syncCount == 0) $
        putStrLn $ "Synced " ++ show syncCount ++ " channel state(s) to disk."


channelMapFromStateFiles ::
    (Serializable v, ToFileName k) =>
    [(k,FilePath)]
    -> IO (DiskMap k v)
channelMapFromStateFiles l = do
    m <- atomically Map.new
    readRes <- forM l maybeReadStateFile
    mapRes <- atomically $ forM readRes
        (either error (\(h,s) -> insertDiskSyncedChannel h s m))
    return m

diskGetStateFiles :: ToFileName k => IO [(k,FilePath)]
diskGetStateFiles = do
    baseDir <- getStateDir
    maybeHashPathL <- map (getHashFileName baseDir) <$> getFileList baseDir
    return [fromJust t | t <- maybeHashPathL, isJust t]

getFileList :: FilePath -> IO [FilePath]
getFileList dir = do
    r <- tryJust (guard . isDoesNotExistError) $ getDirectoryContents dir
    case r of
        Left  e    -> return [] --create dir?
        Right fileList -> return $ filter (\f -> f /= "." && f /= "..") fileList

--TODO: decode
maybeReadStateFile ::
    (Serializable v, ToFileName k) =>
    (k, FileName)
    -> IO (Either String (k, v))
maybeReadStateFile (h,f) = do
    e <- deserialize <$> B.readFile f
    case e of
        Right val -> return $ Right (h,val)
        Left e    -> return $ Left $ "ERROR: Failed to parse state file: " ++ f

getHashFileName :: ToFileName k => FilePath -> FileName -> Maybe (k,FilePath)
getHashFileName baseDir f  =  mkTuple <$>
    validFileName f <*>
    Just (baseDir ++ f)

validFileName :: ToFileName k => FilePath -> Maybe k
validFileName fn = either (const Nothing) Just (fromFileName fn)

mkTuple :: a -> b -> (a,b)
mkTuple a b = (a,b)
----

singleStateDeleteDisk :: ToFileName k => k -> IO ()
singleStateDeleteDisk v = getStateDir >>= removeFile . flip getFileName v

singleStateSyncDisk :: (Serializable v, ToFileName k) => (k, v) -> IO ()
singleStateSyncDisk (k,v) = getStateDir >>= flip writeEntryToFile (k,v)

writeEntryToFile :: (Serializable v, ToFileName k) => String -> (k, v) -> IO ()
writeEntryToFile stateDir (h,s) =
    B.writeFile (getFileName stateDir h) (serializeForDisk s)

getFileName :: ToFileName k => FilePath -> k -> FilePath
getFileName stateDir key =
    stateDir ++ toFileName key

serializeForDisk :: v -> B.ByteString
serializeForDisk = undefined --BL.toStrict . Bin.encode

getStateDir :: IO FilePath
getStateDir = fmap (++ "/state/") getCurrentDirectory

------INTERFACE-----


----disk sync
-- | Sync ChannelMap to disk
syncMapToDisk :: (Serializable v, ToFileName k) => DiskMap k v -> IO Int
syncMapToDisk m = do
    keyActions <- getKeyActions m
    forM_ keyActions (performAction m)
    return $ length keyActions

getKeyActions :: DiskMap k v -> IO [(k, Action)]
getKeyActions m = atomically $
        map (mapSnd itemToAction)
        . filter (\(h,i) -> needsDiskSync i || deleteFromDisk i)
        <$> mapAsList m

mapAsList :: DiskMap k v -> STM [(k, MapItem v)]
mapAsList = LT.toList . Map.stream

performAction :: (Serializable v, ToFileName k) =>  DiskMap k v -> (k, Action) -> IO ()
performAction m (h,Sync)    = syncKeyDataToDisk m h
performAction m (h,Delete)  = singleStateDeleteDisk h >> atomically (Map.delete h m)
performAction m (h,Ignore)  = return ()

syncKeyDataToDisk :: (Serializable v, ToFileName k) =>  DiskMap k v -> k -> IO ()
syncKeyDataToDisk m h = do
    maybeItem <- getMapItemForDiskSync h m
    maybeSyncItemToDisk h maybeItem

-- | Execute item-getter/sync-status-setter and save result to disk
maybeSyncItemToDisk ::
    (Serializable v, ToFileName k) =>
    k
    -> Maybe (MapItem v) -> IO ()
maybeSyncItemToDisk h maybeItem =
    case maybeItem of
        Just i -> singleStateSyncDisk (h, itemContent i)
        Nothing -> return ()

-- | Retrieve map item and simultaneously mark the retrieved item as synced to disk
getMapItemForDiskSync :: ToFileName k => k -> DiskMap k v -> IO (Maybe (MapItem v))
getMapItemForDiskSync h =
    mapAndGetItem h
        (\i@Item {} -> i { needsDiskSync = False })

-- | If a an item exists, read item i, replace in map with (f i), and return
-- | Maybe (f i).
mapAndGetItem :: ToFileName k => k -> (MapItem v -> MapItem v) -> DiskMap k v -> IO (Maybe (MapItem v))
mapAndGetItem h f m =
    atomically $ do
        maybeItem <- Map.lookup h m
        case maybeItem of
            Just item ->
                Map.delete h m >> Map.insert (f item) h m >> return (Just $ f item)
            Nothing -> return Nothing

-- | Retrieve map item and simultaneously mark the retrieved item for disk deletion
markForDeletion :: ToFileName k => k -> DiskMap k v -> IO (Maybe (MapItem v))
markForDeletion h =
    mapAndGetItem h
        (\i@Item {} -> i { deleteFromDisk = True })



-----create/remove interface

-- saved_to_disk

updateItem :: (ToFileName k, Serializable v) => DiskMap k v -> k -> v -> IO ()
updateItem m k v = atomically $ do
    Map.delete k m
    insertItem k v m

removeChannel :: ToFileName k => k -> DiskMap k v -> IO ()
removeChannel k m = atomically $ Map.delete k m
-----create/remove interface



---insert----
insertItem :: ToFileName k => k -> v -> DiskMap k v -> STM ()
insertItem key item = Map.insert
    Item { itemContent = item, needsDiskSync = True, deleteFromDisk = False } key

insertDiskSyncedChannel :: ToFileName k => k -> v -> DiskMap k v -> STM ()
insertDiskSyncedChannel h pcs = Map.insert
    Item { itemContent = pcs, needsDiskSync = False, deleteFromDisk = False } h
---insert----








------- === UTIL === -------

mapSnd f (a,b)= (a,f b)


------- === UTIL === -------
