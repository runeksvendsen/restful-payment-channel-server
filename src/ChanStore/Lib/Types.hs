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
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as BinGet
import qualified Data.Binary.Put as BinPut
import Data.String.Conversions (cs)
import           Control.Concurrent (ThreadId)


data ChannelMap = ChannelMap
    (DiskMap Key ChanState)
    (Maybe (SyncAction,ThreadId))   -- Used when deferred sync is enabled

type Key = HT.OutPoint

data CloseResult  = Closed | DoesntExist
data UpdateResult = WasUpdated | WasNotUpdated

-- |Holds state for payment channel
--  Possible state transitions:
--      * ReadyForPayment -> BeingSettled  (return client change)
--      * ReadyForPayment -> BeingRolled   (free value received by server)
data ChanState =
    ChanOpen     OpenState
  | ChanClosed   CloseState
      deriving Show

data OpenState =
    -- |Payments can be accepted
    ReadyForPayment PaymentState
    -- |Move initiated; waiting for signed settlement tx.
    -- Temporary disruption lasting at most a few seconds.
  | MoveSignWait    ReceiverPaymentChannel


data PaymentState =
    -- |The channel funds have never moved from the original funding outpoint
    NeverMoved      ReceiverPaymentChannel
    -- |Waiting for blockchain confirmations; use both. Temporary redirect.
  | MoveUnconfirmed ReceiverPaymentChannel ReceiverPaymentChannel
    -- |Done; use new. Permanent redirect.
  | MoveConfirmed   HT.OutPoint            ReceiverPaymentChannel


-- |Closing the channel means we return any change left to the client.
data CloseState =
    -- |Ephemeral state: only used while we haven't received a signed settlement tx yet
    CloseSignWait   ReceiverPaymentChannel
    -- |Always ends up here. These can be purged after some time.
  | ChannelClosed   HT.TxHash Payment ReceiverPaymentChannel    -- Save the old channel state in the map for now. We can always purge it later to save space.
      deriving Show

data SettleReason =
    SettleClose
  | SettleMove

-- Needed for Binary instance non-overlap
newtype MaybeChanState = MaybeChanState (Maybe ChanState)


instance Bin.Binary CreateResult where
    put Created = Bin.putWord8 1
    put AlreadyExists = Bin.putWord8 2
    get = Bin.getWord8 >>= \w -> case w of
            1 -> return Created
            2 -> return AlreadyExists
            _ -> fail "unknown byte"

instance Bin.Binary UpdateResult where
    put WasUpdated = Bin.putWord8 1
    put WasNotUpdated = Bin.putWord8 2
    get = Bin.getWord8 >>= \w -> case w of
            1 -> return WasUpdated
            2 -> return WasNotUpdated
            _ -> fail "unknown byte"

instance Bin.Binary CloseResult where
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
    serialize   = BL.toStrict . Bin.encode
    deserialize = deserEither . cs

instance Serializable PaymentChannelState where
    serialize   = BL.toStrict . Bin.encode
    deserialize = deserEither . cs

instance Serializable ChanState where
    serialize   = BL.toStrict . Bin.encode
    deserialize = deserEither . cs

instance Bin.Binary ChanState where
    put (ReadyForPayment s) =
        Bin.putWord8 0x02 >>
        Bin.put s
    put (ChannelSettled txid payment s) =
        Bin.putWord8 0x03 >>
        Bin.put txid >> Bin.put payment >> Bin.put s
    put (SettlementInitiated s) =
        Bin.putWord8 0x04 >>
        Bin.put s

    get = Bin.getWord8 >>=
        (\byte -> case byte of
            0x02    -> ReadyForPayment   <$> Bin.get
            0x03    -> ChannelSettled   <$> Bin.get <*> Bin.get <*> Bin.get
            0x04    -> SettlementInitiated <$> Bin.get
            n       -> fail $ "unknown start byte: " ++ show n)

instance Bin.Binary MaybeChanState where
    put (MaybeChanState (Just chs)) = Bin.put chs
    put (MaybeChanState Nothing  ) = BinPut.putLazyByteString BL.empty

    get = BinGet.isEmpty >>= \empty ->
        if not empty then MaybeChanState . Just <$> Bin.get else return (MaybeChanState Nothing)






