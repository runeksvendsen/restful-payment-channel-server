module Server.ChanStore
(
    module Server.ChanStore.ChanStore,
    init_chanMap,
    sync_chanMap
)
where

import           Server.ChanStore.Types
import Server.ChanStore.ChanStore
import DiskStore


init_chanMap :: String -> IO ChannelMap
init_chanMap storageDirectory  = do
    map <- newChanMap storageDirectory
    chanMapLen <- mapLen map
    putStrLn $ "Restored " ++ show chanMapLen ++ " open channel states from " ++ show storageDirectory
    return map

sync_chanMap :: ChannelMap -> IO ()
sync_chanMap map = do
    putStrLn "Syncing channel map to disk before shutting down..."
    syncCount <- syncMapToDisk map
    putStrLn $ "Synced " ++ show syncCount ++ " channel state(s) to disk."
