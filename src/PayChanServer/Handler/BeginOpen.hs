{-# LANGUAGE OverloadedStrings #-}

module PayChanServer.Handler.BeginOpen where

import qualified PayChanServer.API as API
import qualified PayChanServer.URI as URI
import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import           Servant
import           Data.Maybe (fromMaybe)
import           AppPrelude.Orphans ()


type HostName = String

api = Proxy :: Proxy API.RBPCP
beginOpenAPI = Proxy :: Proxy API.ChanOpen
mkChanURI = safeLink api beginOpenAPI

beginOpenHandler :: SendPubKey -> RecvPubKey -> LockTimeDate -> Maybe HostName -> AppPC ChannelLocation
beginOpenHandler clientPK serverPK lockTime maybeHost = do
--     serverPK <- view Conf.pubKey
--     (Conf.ChanConf _ _ dustLimit _ _) <- view Conf.chanConf
    let cp = MkChanParams clientPK serverPK lockTime -- (Conf.getVal dustLimit)
    (CFundingTxInfo hash vout _) <- blockchainGetConfirmedTxInfo cp
    return ChannelLocation {
        channelInfo_channel_uri = uriPrefix <> cs hostPrefix <>
            cs (show $ URI.mkChanURI clientPK lockTime hash vout)
    }
        where hostPrefix = fromMaybe "" maybeHost ++ "/"
              uriPrefix  = "://"




