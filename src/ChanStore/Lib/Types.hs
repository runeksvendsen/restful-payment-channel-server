module ChanStore.Lib.Types
(
ChanState(..),
CreateResult(..),UpdateResult(..),CloseResult(..),
MapItemResult(..),
MaybeChanState(..),
Key,
ChannelMap(..)
)

where


import           Data.DiskMap (DiskMap, SyncAction,
                            CreateResult(..),
                            Serializable(..), ToFileName(..), Hashable(..), MapItemResult(..))

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannelState, Payment)
import           Data.Bitcoin.PaymentChannel.Util (deserEither)
import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as Bin
import qualified Data.Serialize.Get as BinGet
import qualified Data.Serialize.Put as BinPut
import Data.String.Conversions (cs)
import           Control.Concurrent (ThreadId)


data ChannelMap = ChannelMap
    (DiskMap Key ChanState)
    (Maybe (SyncAction,ThreadId))   -- Used when deferred sync is enabled

type Key = HT.OutPoint

data CloseResult  = Closed | DoesntExist
data UpdateResult = WasUpdated | WasNotUpdated

-- |Holds state for payment channel
data ChanState =
    ReadyForPayment         ReceiverPaymentChannel
  | ChannelSettled          HT.TxHash Payment ReceiverPaymentChannel    -- Save the old channel state in the map for now. We can always purge it later to save space.
  | SettlementInProgress    ReceiverPaymentChannel
      deriving Show


-- Needed for Binary instance non-overlap
newtype MaybeChanState = MaybeChanState (Maybe ChanState)






instance Bin.Serialize CreateResult where
    put Created = Bin.putWord8 1
    put AlreadyExists = Bin.putWord8 2
    get = Bin.getWord8 >>= \w -> case w of
            1 -> return Created
            2 -> return AlreadyExists
            _ -> fail "unknown byte"

instance Bin.Serialize UpdateResult where
    put WasUpdated = Bin.putWord8 1
    put WasNotUpdated = Bin.putWord8 2
    get = Bin.getWord8 >>= \w -> case w of
            1 -> return WasUpdated
            2 -> return WasNotUpdated
            _ -> fail "unknown byte"

instance Bin.Serialize CloseResult where
    put Closed = Bin.putWord8 1
    put DoesntExist = Bin.putWord8 2
    get = Bin.getWord8 >>= \w -> case w of
            1 -> return Closed
            2 -> return DoesntExist
            _ -> fail "unknown byte"



instance ToFileName HT.OutPoint

instance Hashable HT.OutPoint where
    hashWithSalt salt op =
        salt `hashWithSalt` serialize op

instance Serializable HT.OutPoint where
    serialize   = Bin.encode
    deserialize = deserEither . cs

instance Serializable PaymentChannelState where
    serialize   = Bin.encode
    deserialize = deserEither . cs

instance Serializable ChanState where
    serialize   = Bin.encode
    deserialize = deserEither . cs

instance Bin.Serialize ChanState where
    put (ReadyForPayment s) =
        Bin.putWord8 0x02 >>
        Bin.put s
    put (ChannelSettled txid payment s) =
        Bin.putWord8 0x03 >>
        Bin.put txid >> Bin.put payment >> Bin.put s
    put (SettlementInProgress s) =
        Bin.putWord8 0x04 >>
        Bin.put s

    get = Bin.getWord8 >>=
        (\byte -> case byte of
            0x02    -> ReadyForPayment   <$> Bin.get
            0x03    -> ChannelSettled   <$> Bin.get <*> Bin.get <*> Bin.get
            0x04    -> SettlementInProgress <$> Bin.get
            n       -> fail $ "unknown start byte: " ++ show n)

instance Bin.Serialize MaybeChanState where
    put (MaybeChanState (Just chs)) = Bin.put chs
    put (MaybeChanState Nothing  ) = BinPut.putLazyByteString BL.empty

    get = BinGet.isEmpty >>= \empty ->
        if not empty then MaybeChanState . Just <$> Bin.get else return (MaybeChanState Nothing)






