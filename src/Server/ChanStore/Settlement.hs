module Server.ChanStore.Settlement where


import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (unless, when)

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, PaymentChannel(..),
                                                    ChannelParameters(..), BitcoinAmount,
                                                    channelValueLeft)
import           Data.Bitcoin.PaymentChannel.Util (setSenderChangeAddress, BitcoinLockTime)

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT


settle privKey recvAddr txFee pushTx errHandler chanState = do
    -- generate settlement transaction
    let eitherTx = getSettlementBitcoinTx
            chanState (`HC.signMsg` privKey) recvAddr txFee
    tx <- case eitherTx of
        Left e -> errHandler $ show e
        Right tx -> return tx

    -- publish settlement transaction
    eitherTxId <- liftIO $ pushTx tx
    settlementTxId <- case eitherTxId of
        Left e -> errHandler e
        Right txid -> return txid
    return settlementTxId