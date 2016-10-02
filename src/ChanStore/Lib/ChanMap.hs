{-# LANGUAGE FlexibleInstances #-}

module ChanStore.Lib.ChanMap where


import           Data.DiskMap (newDiskMap, SyncAction,
                            addItem, getItem,
                            CreateResult(..),
                            MapItemResult(..),getResult,
                            mapGetItem, updateIfRight,
                            getItemCount,
                            getFilteredKeys,
                            makeReadOnly)

import           Common.Types
import           ChanStore.Lib.Types
import           PayChanServer.Config.Types (ServerDBConf(..))

import           Data.Bitcoin.PaymentChannel.Types
import           Data.Bitcoin.PaymentChannel.Movable

import           Control.Concurrent.STM (STM, atomically)
import           Control.Monad.Catch (finally, handleAll, Exception)
import           Control.Concurrent (forkIO, killThread, threadDelay)

import           Data.Maybe (fromMaybe)


logImportantError = putStrLn

-- Create/destroy
createChanMap :: ServerDBConf -> IO ChannelMap
createChanMap (ServerDBConf syncDir syncInterval) = do
--     if syncInterval == 0 then do
    (map,Nothing) <- newDiskMap syncDir False
    return $ ChannelMap map Nothing
--     else do
--         (map,Just syncAction) <- newDiskMap syncDir True
--         putStrLn $ "INFO: Deferred sync enabled. Syncing every " ++
--             show syncInterval ++ " seconds."
--         threadId <- forkIO $ syncThread syncAction syncInterval
--         return $ ChannelMap map $ Just (syncAction,threadId)

destroyChanMap :: ChannelMap -> IO ()
destroyChanMap (ChannelMap chanMap (Just (syncAction,syncThreadId))) = error "BUG"
--     makeReadOnly chanMap
--     killThread syncThreadId
--     syncNow syncAction
destroyChanMap (ChannelMap _ Nothing) = return ()


openChannelCount :: ChannelMap -> IO Int
openChannelCount (ChannelMap chanMap _) = atomically $
    length <$> getFilteredKeys chanMap isOpen