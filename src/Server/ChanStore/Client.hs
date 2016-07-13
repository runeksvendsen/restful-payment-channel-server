{-|
Module      : Server.ChanStore.Client
Description : ChannelStore client interface
Copyright   : (c) Rune K. Svendsen, 2016
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

.
-}

module Server.ChanStore.Client
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

import Server.ChanStore.ChanStore ( ChanState(..),
                                    getChanState, addChanState,
                                    updateChanState, deleteChanState,
                                    isSettled)
import Server.ChanStore.Connection (ChanMapConn, getConnMap, isOpen)

import Control.Monad.STM

-- |Get item
chanGet conn k   = atomically $
    getConnMap conn >>= (`getChanState` k)

-- |Add item
chanAdd conn k v = atomically $
    getConnMap conn >>= (\map -> addChanState map k v)

-- |Update item
chanUpdate conn k v = atomically $
    getConnMap conn >>= (\map -> updateChanState map k v)

-- |Delete item
chanDelete conn k settleTxId = atomically $
    getConnMap conn >>= (\map -> deleteChanState map k settleTxId)
