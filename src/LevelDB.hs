{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module LevelDB
(
DiskMap,
newDiskMap,
addItem, getItem, updateStoredItem, deleteStoredItem,
getFilteredItems,
mapGetItemCount,
mapGetState

,Serializable(..)

-- TMP
,main
)
where

import           Database.LevelDB.Higher
import           Data.Typeable
import           DiskStore                  (Serializable(..))
import           Control.Exception.Base     (Exception, throw)
import           System.Directory           (doesFileExist)
import           Control.Monad              (unless)
import qualified Data.ByteString as BS


type FileName = String

data DiskMap k v = DiskMap FileName

data ParserError = ParserError BS.ByteString String deriving Typeable
instance Exception ParserError

instance Show ParserError where
    show (ParserError bs e) =
        "LevelDB: Fatal error. Parser error: failed to deserialize value from data (program bug or hardware failure)." ++
        "\nData: " ++ show bs ++
        "\nParser error: " ++ e

main = undefined

runDontCreateLevelDB :: MonadResourceBase m =>
    FilePath -> LevelDBT m a -> m a
runDontCreateLevelDB dbPath = runLevelDB dbPath def{createIfMissing = False} def ""

newDiskMap :: (Serializable k, Serializable v) => FileName -> IO (DiskMap k v)
newDiskMap dbPath = do
    fileExists <- doesFileExist dbPath
    unless fileExists $
        runCreateLevelDB dbPath "" (get "dummykey") >> return ()
    return $ DiskMap dbPath

parseOrError :: Serializable a => BS.ByteString -> a
parseOrError bs =
    case deserialize bs of
        Left e -> throw $ ParserError bs e
        Right v -> v

getItem :: (Serializable k, Serializable v) =>
    DiskMap k v -> k -> IO (Maybe v)
getItem (DiskMap dbPath) key =
    let
        keyBS = serialize key
    in
        runDontCreateLevelDB dbPath (get keyBS) >>=
        \maybeBS -> return (fmap parseOrError maybeBS)

addItem :: (Serializable k, Serializable v) =>
    DiskMap k v -> k -> v -> IO ()
addItem (DiskMap dbPath) key val =
    let
        keyBS = serialize key
        valBS = serialize val
    in
        runCreateLevelDB dbPath "" (put keyBS valBS)


updateStoredItem :: (Serializable k, Serializable v) =>
    DiskMap k v -> k -> v -> IO Bool
updateStoredItem m k v = addItem m k v >> return True   -- Let's, for now, forget about checking whether the item exists or not


deleteStoredItem :: (Serializable k, Serializable v) =>
    DiskMap k v -> k -> IO Bool
deleteStoredItem (DiskMap dbPath) key =
    let
        keyBS = serialize key
    in
        runDontCreateLevelDB dbPath (delete keyBS) >> return True

getFilteredItems :: (Serializable k, Serializable v) =>
    DiskMap k v -> (v -> Bool) -> IO [(k,v)] -- [(BS.ByteString,BS.ByteString)] --[(k,v)]
getFilteredItems (DiskMap dbPath) f =
    let
        scanQuery = queryItems {
              scanInit = [],
              scanFilter = filterQuery f, -- \(kBS,vBS) -> f (parseOrError vBS :: v),
              scanMap = id -- mapQuery --mapQuery :: (Serializable k, Serializable v) => Item -> (k,v)
          }
    in
        map (mapTuple parseOrError parseOrError) <$>
            (runDontCreateLevelDB dbPath $ scan "" scanQuery)

filterQuery :: (Serializable v) =>
    (v -> Bool) -> (BS.ByteString, BS.ByteString) -> Bool
filterQuery f (_,vBS) = f (parseOrError vBS)

mapTuple :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
mapTuple f1 f2 (a,b) = (f1 a, f2 b)

mapGetItemCount :: DiskMap k v -> IO Integer
mapGetItemCount (DiskMap dbPath) =
    runDontCreateLevelDB dbPath $ scan "" queryCount

-- |Atomically: get a value for a key,
--  apply a function to old value to obtain new value,
--  store and return the new value.
mapGetState :: (Serializable k, Serializable v) =>
    DiskMap k v -> (v -> v) -> k -> IO (Maybe v)
mapGetState (DiskMap dbPath) f key =
    let
        keyBS = serialize key
    in runDontCreateLevelDB dbPath $ do
         maybeVal <- fmap parseOrError <$> get keyBS
         let newMaybeVal = fmap f maybeVal
         case newMaybeVal of
            Just val -> put keyBS (serialize val)
            Nothing -> return ()
         return newMaybeVal

