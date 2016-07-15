{-|
Module      : Server.ChanStore.Interface
Description : ChannelStore client interface
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

.
-}

module Server.ChanStore.Interface
(
    chanGet,
    chanAdd,
    chanUpdate,
    chanDelete,

    isSettled,

    ChanMapConn,
    ChanState(..)
)
where

import           Server.ChanStore.Types
import           Server.ChanStore.RequestRunner (Create(..), Get(..), Update(..), Delete(..),
                                             runRequest)
import           Server.ChanStore.ChanStore (isSettled)

import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, Payment)
import qualified Network.Haskoin.Transaction as HT

-- |Add item
chanAdd :: ChanMapConn -> ReceiverPaymentChannel -> IO ()
chanAdd conn rpc = runRequest conn $ Create rpc

-- |Get item
chanGet :: ChanMapConn -> Key -> IO (Maybe ChanState)
chanGet conn key = runRequest conn (Get key) >>=
    \(MaybeChanState maybe) -> return maybe     -- needed to avoid overlapping Binary instance

-- |Update item
chanUpdate :: ChanMapConn -> Key -> Payment -> IO ()
chanUpdate conn key payment = runRequest conn $ Update key payment

-- |Delete item
chanDelete :: ChanMapConn -> Key -> HT.TxHash -> IO ()
chanDelete conn key settleTxId = runRequest conn $ Delete key settleTxId
