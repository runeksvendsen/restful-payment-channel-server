module ChanStore.Init
(
    init_chanMap
)
where

import           ChanStore.Lib.Types
import           ChanStore.Lib.ChanMap


init_chanMap :: String -> IO ChannelMap
init_chanMap storageDirectory  = do
    map <- newChanMap storageDirectory
    openCount <- openChannelCount map
    putStrLn $ "Restored " ++ show openCount ++ " open channel states from " ++ show storageDirectory
    return map