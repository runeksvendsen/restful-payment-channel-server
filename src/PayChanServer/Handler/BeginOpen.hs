module PayChanServer.Handler.BeginOpen where

import qualified PayChanServer.API as API
import qualified PayChanServer.URI as URI
import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import           Servant
-- import           Servant.Utils.Links
import           ChanStore.Orphans ()

api = Proxy :: Proxy API.RBPCP
beginOpenAPI = Proxy :: Proxy API.ChanOpen
mkChanURI = safeLink api beginOpenAPI

beginOpenHandler :: SendPubKey -> BitcoinLockTime -> AppPC ChannelLocation
beginOpenHandler clientPK lockTime = do
    serverPK <- view Conf.pubKey
    (Conf.ChanConf _ _ dustLimit _ _) <- view Conf.chanConf
    let cp = CChannelParameters clientPK serverPK lockTime (Conf.getVal dustLimit)
    (CFundingTxInfo hash vout _) <- blockchainGetConfirmedTxInfo cp
    return ChannelLocation {
        channelInfoChannelUri = cs . show $ URI.mkChanURI clientPK lockTime hash vout
    }




