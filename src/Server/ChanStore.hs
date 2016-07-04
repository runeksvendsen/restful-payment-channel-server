module Server.ChanStore where

import           DiskStore
import           Data.Bitcoin.PaymentChannel.Types
import           Data.Bitcoin.PaymentChannel.Util (deserEither)

import           Data.Hashable (Hashable(..))
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Bin
import qualified Data.Binary.Put as Put

-- |Holds state for open payment channel
data ChanState =
    ReadyForPayment {
        csState          :: ReceiverPaymentChannel
    } |
    ChannelSettled {
        csSettlementTxId :: HT.TxHash
    }

isSettled :: ChanState -> Bool
isSettled (ReadyForPayment _) = False
isSettled (ChannelSettled _) = True

type ChannelMap = DiskMap HT.TxHash ChanState

newChanMap :: FilePath -> IO ChannelMap
newChanMap = newDiskMap


addChanState :: ChannelMap -> HT.TxHash -> ReceiverPaymentChannel -> IO ()
addChanState chanMap key chanState =
    addItem chanMap key (ReadyForPayment chanState)

updateChanState :: ChannelMap -> HT.TxHash -> ReceiverPaymentChannel -> IO Bool
updateChanState chanMap key chanState =
    updateStoredItem chanMap key (ReadyForPayment chanState)

deleteChanState :: ChannelMap -> HT.TxHash -> HT.TxHash -> IO Bool
deleteChanState chanMap key settlementTxId =
    updateStoredItem chanMap key (ChannelSettled settlementTxId)


mapLen = mapGetItemCount

diskSyncThread ::
    (ToFileName k, Serializable v) =>
    DiskMap k v
    -> Int -- ^Sync interval in seconds
    -> IO ()
diskSyncThread m i = putStrLn "Started disk sync thread." >> mapDiskSyncThread m (i * round 1e6)

diskSyncNow ::
    (ToFileName k, Serializable v) =>
    DiskMap k v
    -> IO ()
diskSyncNow = syncToDisk


instance ToFileName HT.TxHash
instance ToFileName HC.Address


instance Hashable HT.OutPoint where
    hashWithSalt salt (HT.OutPoint h i) =
        salt `hashWithSalt` serialize h `hashWithSalt` i

instance Hashable HT.TxHash where
    hashWithSalt salt txid = hashWithSalt salt (serialize txid)

instance Hashable HC.Address where
    hashWithSalt salt addr = hashWithSalt salt (serialize addr)


instance Serializable HT.TxHash where
    serialize   = BL.toStrict . Bin.encode
    deserialize = deserEither

instance Serializable HC.Address where
    serialize   = HC.addrToBase58
    deserialize bs = maybe
        (Left "couldn't deserialize Address")
        Right
        (HC.base58ToAddr bs)

instance Serializable PaymentChannelState where
    serialize   = BL.toStrict . Bin.encode
    deserialize = deserEither

instance Serializable ChanState where
    serialize   = BL.toStrict . Bin.encode
    deserialize = deserEither


instance Bin.Binary ChanState where
    put (ReadyForPayment s) =
        Bin.putWord8 0x02 >>
        Bin.put s

    get = Bin.getWord8 >>=
        (\byte -> case byte of
            0x02    -> ReadyForPayment   <$> Bin.get
            n       -> fail $ "ChanState parser: unknown start byte: " ++ show n)

