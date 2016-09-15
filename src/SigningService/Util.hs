module SigningService.Util where

import           SigningService.Types       (SigningSettleConfig(..))
import           Common.Types

import           PayChanServer.Util
import           PayChanServer.Config.Util
import qualified PayChanServer.Config.Types as Conf

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC


produceSettlementTx ::
    SigningSettleConfig
    -> (ReceiverPaymentChannel, BitcoinAmount)
    -> HT.Tx
produceSettlementTx (SigningSettleConfig privKey recvAddr) (chanState,txFee) =
    getSettlementBitcoinTx chanState (`HC.signMsg` privKey) recvAddr txFee

getSigningSettleConfig :: Conf.Config -> IO SigningSettleConfig
getSigningSettleConfig cfg = SigningSettleConfig <$>
        configLookupOrFail cfg "settlement.privKeySeed" <*>
        configLookupOrFail cfg "settlement.fundsDestinationAddress"
