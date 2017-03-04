{-# LANGUAGE OverloadedStrings #-}

module PayChanServer.Handler.Management.ChannelInfo where

import           AppPrelude.Types
import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import qualified PayChanServer.DB as DB
import           ChanStore.Interface  as DBConn
import           AppPrelude.Man (ChanInfoResult(..))


-- |
getChannelInfo :: SendPubKey -> AppPC ChannelInfo
getChannelInfo clientPK = do
    dbConn <- view Conf.dbInterface
    DB.tryDBRequest (DBConn.manChanInfo dbConn clientPK) >>=
        \res -> case res of
            ChanNotFound ->
                errorWithDescription 404 "Not found"
            OK rpc chanStatus maybeSettleTxId ->
                return ChannelInfo
                    { sender          = getSenderPubKey rpc
                    , status          = chanStatus
                    , value_left      = channelValueLeft rpc
                    , expires         = getExpirationDate rpc
                    , funding_value   = getFundingAmount rpc
                    , funding_address = fundingAddress rpc
                    , funding_source  = getChannelFunding rpc
                    , settlement_txid = maybeSettleTxId
                    }
