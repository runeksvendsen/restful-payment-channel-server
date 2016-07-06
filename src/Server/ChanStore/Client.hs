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
                                    mapLen, isSettled)
import Server.ChanStore.Connection (ChanMapConn, getConnMap, isOpen)

import Control.Monad.STM


chanGet conn k   = atomically $
    getConnMap conn >>= (`getChanState` k)

chanAdd conn k v = atomically $
    getConnMap conn >>= (\map -> addChanState map k v)

chanUpdate conn k v = atomically $
    getConnMap conn >>= (\map -> updateChanState map k v)

chanDelete conn k settleTxId = atomically $
    getConnMap conn >>= (\map -> deleteChanState map k settleTxId)

-- getChanCount conn = atomically $
--     getConnMap conn >>= mapLen