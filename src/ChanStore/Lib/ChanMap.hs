{-# LANGUAGE FlexibleInstances #-}

module ChanStore.Lib.ChanMap where


import           Data.DiskMap (newDiskMap,
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
createChanMap (ServerDBConf syncDir _) = do
    map <- newDiskMap syncDir
    return $ ChannelMap map


openChannelCount :: ChannelMap -> IO Int
openChannelCount (ChannelMap chanMap) = atomically $
    length <$> getFilteredKeys chanMap isOpen