{-# LANGUAGE OverloadedStrings #-}

module Server.Handlers where

import           Prelude hiding (userError)

import           Common.Common
import           Common.Types

import           Server.Util (writeJSON, ChanOpenConfig(..),ChanPayConfig(..),
                              ChanSettleConfig(..),userError,internalError,
                              errorWithDescription, fundingAddrFromParams,
                              getQueryArg,txInfoFromAddr, guardIsConfirmed)
import           BlockchainAPI (TxInfo(..), OutInfo(..), fundingOutInfoFromTxId, txConfs, toFundingTxInfo)
import           Bitcoind (bitcoindNetworkSumbitTx)
import           Server.ChanStore (ChannelMap, ChanState(..))
import           DiskStore (addItem, getItem, updateStoredItem)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (mzero, forM, unless)
import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Monad.State (gets)
import           Snap.Core
-- import           Snap.Util.FileServe
import           Snap.Http.Server
-- import           Snap.Extras.JSON (reqBoundedJSON, writeJSON)

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, ChannelParameters(..), PayChanError(..)
                                                    ,getChannelState, BitcoinAmount, valueToMe,
                                                    channelValueLeft)
import           Data.Bitcoin.PaymentChannel.Util (getFundingAddress, setSenderChangeAddress,
                                                    BitcoinLockTime(..), fromDate)

import qualified Network.Haskoin.Constants as HCC
import Network.Wreq (get, post, asJSON, responseBody)
import Control.Lens ((^.))

import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT

import           Data.Aeson
    (Result(..), Value(Number, Object, String), FromJSON, ToJSON, parseJSON, toJSON,
    fromJSON, withScientific, eitherDecode, eitherDecodeStrict, decode, encode, (.=), (.:), object)

import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Text as T
import Text.Printf (printf)
import Data.EitherR (fmapL)
import Data.String.Conversions (cs)
-- import Data.S (toString)

---server config---
openPrice = 1000 :: BitcoinAmount
minConf = 1 :: Int
minChannelDuration = 12 * 3600 :: Integer
---server config---

applyCORS' :: MonadSnap m => m ()
applyCORS' = do
    modifyResponse $ setHeader "Access-Control-Allow-Origin"    "*"
    modifyResponse $ setHeader "Access-Control-Allow-Methods"   "GET,POST,PUT,DELETE"
    modifyResponse $ setHeader "Access-Control-Allow-Headers"   "Origin,Accept,Content-Type,Payment-Payload"
    modifyResponse $ setHeader "Access-Control-Expose-Headers"  "Location"

--- /fundingInfo ---
writeFundingInfoResp :: MonadSnap m => FundingInfo -> m ()
writeFundingInfoResp fi@(FundingInfo _ _ uri _ _) = do
    applyCORS'
    modifyResponse $ setHeader "Location" (cs uri)
    writeJSON . toJSON $ fi
    writeBS "\n"

mkFundingInfo :: HC.PubKey -> HC.PubKey -> BitcoinLockTime -> FundingInfo
mkFundingInfo recvPK sendPK lockTime  =
    FundingInfo
        recvPK
        (getFundingAddress' sendPK recvPK lockTime)
        (cs $ channelOpenURL hOSTNAME sendPK lockTime)
        openPrice
        minConf

logFundingInfo :: MonadSnap m => m ()
logFundingInfo  = do
    pk <- getQueryArg "client_pubkey"
    exp <- getQueryArg "exp_time"
    liftIO . putStrLn $ printf
        "Got fundingInfo request: pubkey=%s exp=%s"
            (show (pk :: HC.PubKey))
            (show (exp :: BitcoinLockTime))

--- /fundingInfo ---


----Settlement----
chanSettle :: MonadSnap m => ChanSettleConfig -> m ()
chanSettle (SettleConfig privKey recvAddr txFee chanMap hash vout payment) = do
    liftIO . putStrLn $ printf
        "Processing settlement request for channel %s/%d: "
            (cs $ HT.txHashToHex hash :: String) vout ++ show payment

    chanState <- getChannelStateOr404 chanMap hash

    let eitherTx = getSettlementBitcoinTx
            chanState (`HC.signMsg` privKey) recvAddr txFee
    tx <- case eitherTx of
        Left e -> internalError $ show e
        Right tx -> return tx

    eitherTxId <- liftIO $ bitcoindNetworkSumbitTx tx
    txid <- case eitherTxId of
        Left e -> internalError e
        Right txid -> return txid
    modifyResponse $ setResponseStatus 200 (C.pack "Channel closed")
---


----Payment----

chanPay :: MonadSnap m => ChanPayConfig -> m ()
chanPay (PayConfig chanMap hash vout maybeNewAddr payment) = do
    liftIO . putStrLn $ printf
        "Processing payment request for channel %s/%d: "
            (cs $ HT.txHashToHex hash :: String) vout ++ show payment

    existingChanState <- maybeUpdateChangeAddress maybeNewAddr =<<
            getChannelStateOr404 chanMap hash

    (valReceived,newChanState) <- case recvPayment existingChanState payment of
        Right (amount,state) -> return (amount,state)
        Left BadPayment -> userError "Bad payment amount"
        Left BadSignature -> userError "Bad payment signature"

    liftIO $ updateStoredItem chanMap hash (ReadyForPayment newChanState)
    writeJSON . toJSON $ PaymentResult {
            paymentResultchannel_value_left = channelValueLeft newChanState,
            paymentResultvalue_received = valReceived
        }

getChannelStateOr404 :: MonadSnap m => ChannelMap -> HT.TxHash -> m ReceiverPaymentChannel
getChannelStateOr404 chanMap hash =
    liftIO (getItem chanMap hash) >>=
    maybe
        (errorWithDescription 404 "No such channel")
        (return . csState)

maybeUpdateChangeAddress :: MonadSnap m =>
    Maybe HC.Address
    -> ReceiverPaymentChannel
    -> m ReceiverPaymentChannel
maybeUpdateChangeAddress maybeAddr state =
    maybe (return state) updateAddressAndLog maybeAddr
        where updateAddressAndLog addr = do
                liftIO . putStrLn $ "Updating client change address to " ++ toString addr
                return $ setSenderChangeAddress state addr

----Payment----




--- POST /channels/ ---
channelOpenHandler :: MonadSnap m =>
    (HC.PrvKey,HC.PubKey) --TODO: move to config
    -> ChanOpenConfig
    -> m ()
channelOpenHandler (prvKeyServ,pubKeyServ)
    (OpenConfig chanMap txInfo@(TxInfo txId _ (OutInfo _ chanVal idx)) sendPK sendChgAddr lockTime payment) = do
    liftIO . putStrLn $ "Processing channel open request... " ++
        show (sendPK, lockTime, txInfo, payment)

    --New channel creation--
    let eitherChanState = channelFromInitialPayment
            (CChannelParameters sendPK pubKeyServ lockTime)
            (toFundingTxInfo txInfo) sendChgAddr payment

    recvChanState <- case eitherChanState of
            Right s -> return s
            Left BadPayment -> userError "Bad payment amount"
            Left BadSignature -> userError "Bad payment signature"
    liftIO $ addItem chanMap txId (ReadyForPayment recvChanState)
    --New channel creation--

    --Send success response--
    modifyResponse $ setHeader "Location" (cs $ activeChannelURL hOSTNAME txId idx)
    modifyResponse $ setResponseStatus 201 (C.pack "Channel ready")
    writeJSON . toJSON $ PaymentResult {
        paymentResultchannel_value_left = channelValueLeft recvChanState,
        paymentResultvalue_received = valueToMe recvChanState
    }
    --Send success response--

    liftIO (putStrLn "Created new payment channel state: ")
    liftIO (print recvChanState)


blockchainGetFundingInfo :: MonadSnap m => m TxInfo
blockchainGetFundingInfo =
    fundingAddrFromParams <$>
        getQueryArg "client_pubkey" <*>
        getQueryArg "exp_time"
    >>= txInfoFromAddr >>= guardIsConfirmed (fromIntegral minConf)
--- POST /channels/ ---


