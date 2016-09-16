module PayChanServer.Handler.FundingInfo where

import           PayChanServer.Types
import           PayChanServer.Util
import qualified PayChanServer.Config.Types as Conf
import           Data.Bitcoin.PaymentChannel.Util   (getFundingAddress, getRedeemScript)


fundingInfoHandler :: SendPubKey -> BitcoinLockTime -> AppPC FundingInfo
fundingInfoHandler clientPK lockTime = do
    serverPK <- view Conf.pubKey
    (Conf.ChanConf btcMinConf openPrice dustLimitT settlePeriod minDuratn) <- view Conf.chanConf
    let dustLimit = Conf.getVal dustLimitT
    let chanParams = CChannelParameters clientPK serverPK lockTime dustLimit
    return $ FundingInfo serverPK dustLimitT (getFundingAddress chanParams)
             (getRedeemScript chanParams) openPrice btcMinConf settlePeriod minDuratn



-- , fundingInfoFundingAddressCopy       = cs . pathParamEncode $ getFundingAddress chanParams
--       , fundingInfoRedeemScriptCopy         = cs . hexEncode $ getRedeemScript chanParams
--       , fundingInfoOpenPrice                = fromIntegral openPrice
--       , fundingInfoFundingTxMinConf         = fromIntegral $ Conf.getVal btcMinConf
--       , fundingInfoSettlementPeriodHours    = fromIntegral $ Conf.getVal settlePeriod
--       , fundingInfoMinDurationHours         = fromIntegral $ Conf.getVal minDuratn