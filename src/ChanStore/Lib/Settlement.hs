module ChanStore.Lib.Settlement where --TODO: move

import           ChanStore.Lib.Types hiding (UpdateResult(..))
import           DiskStore (getAllItems,
                            mapGetItem, mapGetItems, MapItemResult(..),
                            getFilteredKV, getFilteredItems,
                            DiskMap(..), Serializable(..), ToFileName(..), Hashable(..))

import           PayChanServer.Types (ServerSettleConfig(..), SigningSettleConfig(..))
import           PayChanServer.Util (internalError)
import           Bitcoind (BTCRPCInfo, bitcoindNetworkSumbitTx)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    BitcoinLockTime(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft, PayChanError)
import           Data.Bitcoin.PaymentChannel.Util (setSenderChangeAddress, BitcoinLockTime)

import qualified STMContainers.Map as Map
import qualified ListT as LT
import           Control.Concurrent.STM (STM, atomically)
import           Control.Exception.Base     (Exception, throw)
import           Data.Time.Clock (UTCTime)

import qualified Network.Haskoin.Transaction as HT


finishSettlingChannel :: ChannelMap -> (Key,HT.TxHash) -> IO (MapItemResult Key ChanState)
finishSettlingChannel m (k,settleTxId)= do
    head <$> mapGetItems m finishSettling (return [k])
    where
        finishSettling (SettlementInProgress rpc) = Just $
            ChannelSettled settleTxId (getNewestPayment rpc) rpc
        finishSettling _ = Nothing

beginSettlingChannel :: ChannelMap -> Key -> IO ReceiverPaymentChannel
beginSettlingChannel m k = head <$> beginSettlingChannels m (return [k])

-- Time-based settlement {

-- |Return a list of 'ReceiverPaymentChannel's expiring before specified point in time,
--      and simultanesouly mark them as in the process of being settled ('SettlementInProgress')
beginSettlingExpiringChannels :: ChannelMap -> UTCTime -> IO [ReceiverPaymentChannel]
beginSettlingExpiringChannels m currentTimeIsh =
    beginSettlingChannels m $
    fmap fst <$>    --get Key
        openChannelsExpiringBefore currentTimeIsh m

-- |Just return a list of 'ReceiverPaymentChannel's expiring before specified point in time
justRetrieveExpiringChannels :: ChannelMap -> UTCTime -> IO [ReceiverPaymentChannel]
justRetrieveExpiringChannels m currentTimeIsh =
    atomically $
        fmap (gatherPayChan . snd) <$>    --get Value
            openChannelsExpiringBefore currentTimeIsh m

beginSettlingChannels :: ChannelMap -> STM [Key] -> IO [ReceiverPaymentChannel]
beginSettlingChannels m keyGetterAction = do
    res <- mapGetItems m markAsSettlingIfOpen $ keyGetterAction
    return $ map gatherFromResults res
    where
          markAsSettlingIfOpen (ReadyForPayment rpc)    = Just $ SettlementInProgress rpc
          -- Should never be reached, as we're fetching the relevant keys and their corresponding items atomically
          markAsSettlingIfOpen (SettlementInProgress _) = Nothing
          markAsSettlingIfOpen (ChannelSettled _ _ _)     = Nothing

-- | Important note: does not support 'LockTimeBlockHeight'. The library supports
--      'LockTimeBlockHeight', but the protocol does not.
-- Get keys for all channel states with an expiration date later than the specified 'UTCTime'
openChannelsExpiringBefore :: UTCTime -> ChannelMap -> STM [(Key,ChanState)]
openChannelsExpiringBefore currentTimeIsh m = getFilteredKV m isOpenAndExpiresBefore
    where
        isOpenAndExpiresBefore (ReadyForPayment cs) = chanExpiresBefore cs
        isOpenAndExpiresBefore _ = False
        chanExpiresBefore = expiresEarlierThan currentTimeIsh . getExpirationDate

expiresEarlierThan :: UTCTime -> BitcoinLockTime -> Bool
expiresEarlierThan _        (LockTimeBlockHeight _) = error "LockTimeBlockHeight not supported"
expiresEarlierThan circaNow (LockTimeDate expDate) = circaNow > expDate
-- Time-based settlement }

gatherFromResults :: MapItemResult HT.OutPoint ChanState -> ReceiverPaymentChannel
gatherFromResults res = case res of
      (ItemUpdated _ cs) -> gatherPayChan cs
      NotUpdated -> error "BUG: Should not be possible since irrelevant keys have been filtered off"
      NoSuchItem -> error "BUG: Tried to mark non-existing channel state item as settling"

gatherPayChan :: ChanState -> ReceiverPaymentChannel
gatherPayChan cs = case cs of
    (SettlementInProgress rpc) -> rpc
    _ -> error "BUG: 'markAsSettlingIfOpen' should only update an item to 'SettlementInProgress'"

