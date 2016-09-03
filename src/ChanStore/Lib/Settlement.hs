module ChanStore.Lib.Settlement where --TODO: move

import           ChanStore.Lib.Types hiding (UpdateResult(..))
import           Data.DiskMap (getAllItems,
                            mapGetItem, mapGetItems, MapItemResult(..),
                            getFilteredKV, getFilteredItems, collectSortedItemsWhile,
                            DiskMap, Serializable(..), ToFileName(..), Hashable(..))

import           PayChanServer.Types (ServerSettleConfig(..), SigningSettleConfig(..))



import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    BitcoinLockTime(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft, PayChanError)
import           Data.Bitcoin.PaymentChannel.Util (setSenderChangeAddress, BitcoinLockTime)

import qualified Util
import qualified STMContainers.Map as Map
import qualified ListT as LT
import           Data.Ord (comparing, Down(Down))
import           Control.Concurrent.STM (STM, atomically)
import           Control.Exception.Base     (Exception, throw)
import           Data.Time.Clock (UTCTime)
import qualified Data.List as List

import qualified Network.Haskoin.Transaction as HT

-- |Mark the channel in question as settled by providing its key and the TxId of the settlement transaction
finishSettlingChannel :: ChannelMap -> (Key,HT.TxHash) -> IO (MapItemResult Key ChanState)
finishSettlingChannel (ChannelMap m _) (k,settleTxId) =
    head <$> mapGetItems m finishSettling (return [k])
    where
        finishSettling (SettlementInProgress rpc) = Just $
            ChannelSettled settleTxId (getNewestPayment rpc) rpc
        finishSettling _ = Nothing

beginSettlingChannel :: ChannelMap -> Key -> IO ReceiverPaymentChannel
beginSettlingChannel chanMap k = head <$> beginSettlingChannels chanMap (return [k])

-- |Given a list of Keys in STM, retrieve the channel states in question from the map while
-- simultaneously marking them as in the process of being settled.
beginSettlingChannels :: ChannelMap -> STM [Key] -> IO [ReceiverPaymentChannel]
beginSettlingChannels (ChannelMap m _) keyGetterAction = do
    res <- mapGetItems m markAsSettlingIfOpen keyGetterAction
    return $ map gatherFromResults res
    where
          markAsSettlingIfOpen (ReadyForPayment rpc)  = Just $ SettlementInProgress rpc
          -- Should never be reached, as we're fetching the relevant keys and their corresponding items atomically
          markAsSettlingIfOpen SettlementInProgress{} = Nothing
          markAsSettlingIfOpen ChannelSettled{}       = Nothing

-- Time-based settlement {

-- |Return a list of 'ReceiverPaymentChannel's expiring before specified point in time,
--      and simultanesouly mark them as in the process of being settled ('SettlementInProgress')
beginSettlingExpiringChannels :: ChannelMap -> UTCTime -> IO [ReceiverPaymentChannel]
beginSettlingExpiringChannels chanMap currentTimeIsh =
    beginSettlingChannels chanMap $
    fmap fst <$>    --get Key
        openChannelsExpiringBefore currentTimeIsh chanMap

-- | Important note: does not support 'LockTimeBlockHeight'. The library supports
--      'LockTimeBlockHeight', but the protocol does not.
-- Get keys for all channel states with an expiration date later than the specified 'UTCTime'
openChannelsExpiringBefore :: UTCTime -> ChannelMap -> STM [(Key,ChanState)]
openChannelsExpiringBefore currentTimeIsh (ChannelMap m _) =
    getFilteredKV m isOpenAndExpiresBefore
    where
        isOpenAndExpiresBefore (ReadyForPayment cs) = chanExpiresBefore cs
        isOpenAndExpiresBefore _ = False
        chanExpiresBefore = expiresEarlierThan currentTimeIsh . getExpirationDate

expiresEarlierThan :: UTCTime -> BitcoinLockTime -> Bool
expiresEarlierThan _        (LockTimeBlockHeight _) = error "LockTimeBlockHeight not supported"
expiresEarlierThan circaNow (LockTimeDate expDate) = circaNow > expDate
-- Time-based settlement }

-- Value-based settlement {
beginSettlingChannelsByValue :: ChannelMap -> BitcoinAmount -> IO [ReceiverPaymentChannel]
beginSettlingChannelsByValue chanMap minValue =
    beginSettlingChannels chanMap $
    fmap getKeyFromItem <$>
        fewestChannelsCoveringValue chanMap minValue
    where
        getKeyFromItem = getChannelID

-- |Return a list of 'ReceiverPaymentChannel's which, together in total, have received at least
--  'minValue'. If 'minValue' is greater than all available value, all channels are returned.
--  Ie. you have to make sure you don't request too much value.
fewestChannelsCoveringValue :: ChannelMap -> BitcoinAmount -> STM [ReceiverPaymentChannel]
fewestChannelsCoveringValue  (ChannelMap m _) minValue =
    Util.accumulateWhile collectUntilEnoughValue .
        List.sortBy descendingValueReceived .
        fmap getOpenChanState <$> getFilteredItems m isOpen
    where
        isOpen (ReadyForPayment _) = True
        isOpen _                   = False
        collectUntilEnoughValue accumulatedItems _ =
            sum (map valueToMe accumulatedItems) < minValue
        -- 'Down' reverses standard (ascending) sort order
        descendingValueReceived = comparing $ Down . fromIntegral . valueToMe
        getOpenChanState (ReadyForPayment cs) = cs
        getOpenChanState _                    =
            error "BUG: Non-open channels should have been filtered off"
-- Value-based settlement }

gatherFromResults :: MapItemResult HT.OutPoint ChanState -> ReceiverPaymentChannel
gatherFromResults res = case res of
      (ItemUpdated _ cs) -> gatherPayChan cs
      NotUpdated -> error "BUG: Should not be possible since irrelevant keys have been filtered off"
      NoSuchItem -> error "BUG: Tried to mark non-existing channel state item as settling"

gatherPayChan :: ChanState -> ReceiverPaymentChannel
gatherPayChan cs = case cs of
    (SettlementInProgress rpc) -> rpc
    _ -> error "BUG: 'markAsSettlingIfOpen' should only update an item to 'SettlementInProgress'"

