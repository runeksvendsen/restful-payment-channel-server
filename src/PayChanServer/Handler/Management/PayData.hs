{-# LANGUAGE OverloadedStrings #-}

module PayChanServer.Handler.Management.PayData where


import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import qualified PayChanServer.DB as DB
import           ChanStore.Interface  as DBConn
import           ChanStore.Lib.Types (DataPayloadResult(..), UpdateResult(..))
import           Servant.API

-- |
setupDataPayload :: SendPubKey -> BitcoinAmount -> JSONString -> AppPC NoContent
setupDataPayload clientPK payValue dataPayload = do
    dbConn <- view Conf.dbInterface
    DB.tryDBRequest (DBConn.manPayData dbConn clientPK payValue dataPayload) >>=
        \res -> case res of
            OK ->
                return NoContent
            RegisterPayloadError NoSuchChannel      ->
                errorWithDescription 404 "No such channel"
            RegisterPayloadError (ChanClosed _ _)   ->
                errorWithDescription 410 "Channel closed or being closed"
            RegisterPayloadError ChanBeingClosed    ->
                errorWithDescription 410 "Channel closed or being closed"





