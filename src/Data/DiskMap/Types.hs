module Data.DiskMap.Types where

import Data.Hashable
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import Control.Monad (forM, filterM, when)
import qualified  STMContainers.Map as Map
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception.Base     (Exception)
import Control.Concurrent.MVar (MVar)


-- |
data DiskMap k v = DiskMap
    MapConfig
    (STMMap k v)
    (SyncState k)
    (TVar Bool)     -- Read-only

type STMMap k v = Map.Map k (MapItem v)

data MapItem v = Item {
    itemContent :: v
    ,needsDiskSync          :: Bool      -- DEPRECATED
    ,isBeingDeletedFromDisk :: Bool }
    deriving Show

data MapConfig = MapConfig FilePath Bool

data SyncState k = SyncState
    (Map.Map k SyncOp)  -- Deferred sync map
    (MVar ())           -- If empty: sync is in progress

-- |Disk sync IO action. Returns the number of disk items that were
--  either updated or deleted.
type SyncAction = IO Integer

data CreateResult = Created | AlreadyExists
data WriteException = PermissionDenied deriving (Show, Eq)
instance Exception WriteException

-- |
data MapItemResult k v =
    ItemUpdated k v |
    NotUpdated |
    NoSuchItem

-- | Types that can be serialized and deserialized
class Serializable a where
    serialize   :: a -> BS.ByteString
    deserialize :: BS.ByteString -> Either String a

-- | Types that can be converted to a unique filename
class (Serializable k, Eq k, Hashable k) => ToFileName k where
    toFileName      :: k -> String
    fromFileName    :: String -> Either String k

    toFileName = C.unpack . B16.encode . serialize
    fromFileName = deserialize . fst . B16.decode . C.pack

type Success = Bool

-- Sync
data SyncOp = Sync | Delete


