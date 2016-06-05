{-# LANGUAGE  OverloadedStrings #-}

module Client where

import Common.Common
import Common.Types (FundingInfo(..), ChanOpenResult(..))
import BlockchainAPI (waitGetFundingInfo, TxInfo(..), toFundingTxInfo)

import qualified Data.Bitcoin.PaymentChannel as PayChan (channelWithInitialPaymentOf, sendPayment)
import           Data.Bitcoin.PaymentChannel.Types (SenderPaymentChannel, ChannelParameters(..), Payment, BitcoinLockTime)
import           Data.Bitcoin.PaymentChannel.Util (parseBitcoinLocktime, fromDate)
-- import           Data.Bitcoin.PaymentChannel.Util (getFundingAddress, BitcoinLockTime(..))

import Control.Monad (unless)
import           Control.Monad.IO.Class
import qualified Crypto.Secp256k1 as Secp
import           Data.Aeson
    (Result(..), Value(Number), FromJSON, ToJSON, parseJSON, toJSON,
    fromJSON, withScientific, eitherDecode, eitherDecodeStrict, encode)
import Data.Maybe (fromJust)
import Data.Scientific
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Network.Wreq (get, post, put, delete, asJSON, responseBody)
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Constants as HCC
import Data.Time.Clock (getCurrentTime, secondsToDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Control.Lens ((^.))
import Data.Word (Word32)
import Data.EitherR (fmapL)
import Data.String.Conversions (cs)
import qualified Data.Text as T

import Debug.Trace

type URI = T.Text

pubKeyClient = HC.derivePubKey prvKeyClient
prvKeyClient = HC.makePrvKey $ fromJust $ Secp.secKey $ fromHexString
    "aa519d456af73955295a161e096c10610b98d3d21404380edc4fa378dc58c1d7"

cHANNEL_DURATION = 3 * 24 * 3600 :: Word32

httpGETParseJSON :: FromJSON a => String -> IO (Either String a)
httpGETParseJSON url = fmap (eitherDecode . (^. responseBody)) (get url)

httpPOSTParseJSON :: (ToJSON a, FromJSON b) => T.Text -> a -> IO (Either String b)
httpPOSTParseJSON url obj = eitherDecode . (^. responseBody) <$>
    post (cs url) (toJSON obj)

failOnError = either (fail . ("failed to parse response: " ++)) return

main = do
    HCC.switchToTestnet3
    expTime <- durationToLockTime cHANNEL_DURATION
    (chanURI,state) <- httpGETFundingInfo pubKeyClient expTime >>=
            createChannel pubKeyClient expTime
    let (pmnt,newstate) = PayChan.sendPayment state 12000
    updatePayment chanURI pmnt

httpGETFundingInfo :: HC.PubKey -> BitcoinLockTime -> IO FundingInfo
httpGETFundingInfo sendPK expTime =
    failOnError =<<
        httpGETParseJSON (fundingInfoURL "localhost:8000" sendPK expTime)

createChannel :: HC.PubKey -> BitcoinLockTime -> FundingInfo -> IO (URI,SenderPaymentChannel)
createChannel sendPK expTime (FundingInfo recvPK fundAddrSrv chanCreateURI openPrice minConf) = do
    let fundAddr = getFundingAddress' sendPK recvPK expTime
    unless (fundAddr == fundAddrSrv) $
        fail "BUG: server's calculated funding address and ours don't match"

    printFundingInfo (toString fundAddr) minConf
    txInfo@(TxInfo txId _ outInfo) <- waitGetFundingInfo (toString fundAddr) (fromIntegral minConf)
    putStrLn $ "Got funding tx info: " ++ show txInfo

    let chanParams = CChannelParameters sendPK recvPK expTime
    let (payment,chanState) = PayChan.channelWithInitialPaymentOf chanParams
            (toFundingTxInfo txInfo) (`HC.signMsg` prvKeyClient) (HC.pubKeyAddr sendPK) openPrice

    openRes <- failOnError =<< httpPOSTParseJSON chanCreateURI (toJSON payment)
    let newChanURI = chanOpenResultchannel_uri openRes
    return (newChanURI,chanState)

updatePayment ::
    URI
    -> Payment
    -> IO ()
updatePayment url payment = do
    finRes <- put (cs url) (toJSON payment)
    putStrLn $ "updatePayment: Got some response: " ++ show finRes


printFundingInfo addrStr conf =
    putStrLn $ "To fund the channel, pay to address " ++
        addrStr ++ ",\n\t" ++
        "and then wait for " ++ show conf ++ " confirmations until making payments."

-- | Derive channel expiration date from current time + requested duration
durationToLockTime duration =
    let
        getPOSIXTimeSeconds = fmap (fromIntegral . truncate) getPOSIXTime
        getExpirationTime duration = fmap (+ duration) getPOSIXTimeSeconds
    in
        parseBitcoinLocktime <$> getExpirationTime duration