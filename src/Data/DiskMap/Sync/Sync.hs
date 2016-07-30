module Data.DiskMap.Sync.Sync where

import Data.DiskMap.Types
import Data.DiskMap.Internal.Helpers (insertDiskSyncedChannel)

import System.Directory (getDirectoryContents, removeFile, doesFileExist)
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.ByteString as BS
import Control.Monad.STM
import Control.Exception (IOException)
import Control.Monad.Catch (try)
import Control.Monad (forM, filterM, when)
import Control.Concurrent.Spawn (parMapIO)
import qualified  STMContainers.Map as Map


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