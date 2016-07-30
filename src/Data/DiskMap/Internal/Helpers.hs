module Data.DiskMap.Internal.Helpers where

import Data.DiskMap.Types

import Control.Monad.STM
import qualified  STMContainers.Map as Map


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