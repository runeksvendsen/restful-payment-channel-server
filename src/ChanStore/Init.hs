module ChanStore.Init
(
    init_chanMap
-- ,   destroy_chanMap
)
where

import           ChanStore.Lib.Types
import           ChanStore.Lib.ChanMap
import           PayChanServer.Config.Types (ServerDBConf(..))


init_chanMap :: ServerDBConf -> IO ChannelMap
init_chanMap conf@(ServerDBConf storageDirectory _) = do
    map <- createChanMap conf
    openCount <- openChannelCount map
    putStrLn $ "Restored " ++ show openCount ++ " open channel states from " ++ show storageDirectory
    return map
