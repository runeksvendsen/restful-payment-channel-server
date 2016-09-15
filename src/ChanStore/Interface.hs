{-|
Module      : Lib.Interface
Description : ChannelStore client interface
Copyright   : Rune K. Svendsen
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

Potato
-}

module ChanStore.Interface
(
    New.Interface(chanAdd , chanGet , chanUpdate
            , settleByExpBegin
            , settleByIdBegin , settleFin)
  , New.mkChanStoreInterface
  , ConnManager
  , ChanState(..)
  , UpdateResult(..)
)
where

-- Don't use the stuff below
import qualified ChanStore.API as New

import           ConnManager.RequestRunner (ConnManager, runRequest)

import           ChanStore.Lib.Types
import           ChanStore.Spec

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment)
import           Data.Time.Clock (UTCTime)
import qualified Network.Haskoin.Transaction as HT

mkChanStoreInterface :: ConnManager -> Interface
mkChanStoreInterface connMan =
    Interface
        (chanAdd'           connMan)
        (chanGet'           connMan)
        (chanUpdate'        connMan)
        (settleByExpBegin'  connMan)
        (settleByIdBegin'   connMan)
        (settleFin'         connMan)

data Interface = Interface {
    chanAdd             :: ReceiverPaymentChannel -> IO CreateResult
  , chanGet             :: Key -> IO (Maybe ChanState)
  , chanUpdate          :: Key -> Payment -> IO UpdateResult
  , settleByExpBegin    :: UTCTime -> IO [ReceiverPaymentChannel]
  , settleByIdBegin     :: Key -> IO ReceiverPaymentChannel
  , settleFin           :: Key -> HT.TxHash -> IO ()
}


-- |Add item
chanAdd' :: ConnManager -> ReceiverPaymentChannel -> IO CreateResult
chanAdd' conn rpc = runRequest conn $ Create rpc

-- |Get item
chanGet' :: ConnManager -> Key -> IO (Maybe ChanState)
chanGet' conn key = runRequest conn (Get key) >>=
    \(MaybeChanState maybeCS) -> return maybeCS

-- |Update item
chanUpdate' :: ConnManager -> Key -> Payment -> IO UpdateResult
chanUpdate' conn key payment = runRequest conn $ Update key payment

-- |Get zero or more channel objects based on expiration date and simultaneously begin settlement
settleByExpBegin' :: ConnManager -> UTCTime -> IO [ReceiverPaymentChannel]
settleByExpBegin' conn expiringBefore =
    runRequest conn $ ByExpSettleBegin expiringBefore

-- |Get single channel object based on channel ID and simultaneously begin settlement
settleByIdBegin' :: ConnManager -> Key -> IO ReceiverPaymentChannel
settleByIdBegin' conn key =
    runRequest conn $ ByIdSettleBegin key

-- |Finish settlement for single channel object based on channel ID
settleFin' :: ConnManager -> Key -> HT.TxHash -> IO ()
settleFin' conn key settleTxId = runRequest conn $ SettleFin key settleTxId
