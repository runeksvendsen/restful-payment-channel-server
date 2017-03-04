module PayChanServer.Handler.FundingInfo where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import           PaymentChannel.Util   (getFundingAddress, getRedeemScript)


fundingInfoHandler :: SendPubKey -> LockTimeDate -> AppPC FundingInfo
fundingInfoHandler clientPK lockTime = do
    -- TODO: Get pubkey from BitcoinSigner
    serverPK <- view Conf.pubKey
    (Conf.ChanConf btcMinConf openPrice dustLimitT settlePeriod minDuratn) <- view Conf.chanConf
    let dustLimit = getVal dustLimitT
    let chanParams = MkChanParams clientPK serverPK lockTime
    return $ FundingInfo serverPK dustLimit (getFundingAddress chanParams)
             (getRedeemScript chanParams)
             (getVal openPrice) (getVal btcMinConf) (getVal settlePeriod) (getVal minDuratn)
     where getVal = Conf.getVal

