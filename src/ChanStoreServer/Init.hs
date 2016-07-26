module ChanStoreServer.Init
(
    init_chanMap
)
where

import           ChanStoreServer.ChanStore.Types
import           ChanStoreServer.ChanStore.ChanStore


init_chanMap :: String -> IO ChannelMap
init_chanMap storageDirectory  = do
    map <- newChanMap storageDirectory
    chanMapLen <- mapLen map
    putStrLn $ "Restored " ++ show chanMapLen ++ " open channel states from " ++ show storageDirectory
    return map