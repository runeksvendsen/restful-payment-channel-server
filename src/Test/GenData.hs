{-# LANGUAGE  OverloadedStrings #-}

module Test.GenData where

import Common.Common    (mkOpenPath, mkPaymentPath)
import           Data.Bitcoin.PaymentChannel (channelWithInitialPaymentOf, sendPayment)
import           Data.Bitcoin.PaymentChannel.Util (BitcoinLockTime(..), parseBitcoinLocktime,
                                                   getFundingAddress)
import           Data.Bitcoin.PaymentChannel.Types
    (Payment, ChannelParameters(..), FundingTxInfo(..))
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Constants as HCC
import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import           Data.Aeson         (object, ToJSON, toJSON, (.=), encode)
import Data.Maybe (isJust, fromJust)
import Data.Word (Word32)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import System.Entropy (getEntropy)
import Crypto.Secp256k1 (secKey)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Server.Config (pubKeyServer, fundsDestAddr, openPrice)
import BlockchainAPI.Types (TxInfo(..), OutInfo(..))

mockChangeAddress = fundsDestAddr
nUM_PAYMENTS = 10 :: Int
cHAN_DURATION = 3600 * 24 * 7 :: Integer




main = do
    HCC.switchToTestnet3
    genData ""

genData endpoint = do
    privSeed <- getEntropy 32
    prvKey <- case fmap HC.makePrvKey (secKey privSeed) of
            Nothing -> fail "couldn't derive secret key from seed"
            Just k -> return k
    expTime <- fmap round getPOSIXTime
    let sess = genChannelSession endpoint nUM_PAYMENTS prvKey pubKeyServer
            (parseBitcoinLocktime $ fromIntegral $ expTime + cHAN_DURATION)

    BS.putStr (cs . encode . toJSON . getSessionData $ sess)



data ChannelSession = ChannelSession {
    csEndPoint    :: T.Text,
    csParams      :: ChannelParameters,
    csFundAddr    :: HC.Address,
    csInitPayment :: Payment,
    csPayments    :: [Payment]
}

-- | Generate payment of value 1 satoshi
iterateFunc (p,s) = sendPayment s 1

genChannelSession :: T.Text -> Int -> HC.PrvKey -> HC.PubKey -> BitcoinLockTime -> ChannelSession
genChannelSession endPoint numPayments privClient pubServer expTime =
    let
        cp = CChannelParameters (HC.derivePubKey privClient) pubServer expTime
        fundInfo = deriveMockFundingInfo cp
        (initPay,initState) = channelWithInitialPaymentOf cp fundInfo
                (`HC.signMsg` privClient) mockChangeAddress 1000
        payStateList = tail $ iterate iterateFunc (initPay,initState)
        payList = map fst payStateList
    in
        ChannelSession endPoint cp (getFundingAddress cp) initPay (take numPayments payList)
--------

data PaySessionData = PaySessionData {
    openURL     :: T.Text,
    payURLList  :: [T.Text]
}

instance ToJSON PaySessionData where
    toJSON (PaySessionData oURL pURLs) =
        object [ "open_url" .= oURL, "payment_urls" .= pURLs ]


getSessionData :: ChannelSession -> PaySessionData
getSessionData (ChannelSession endPoint cp fundAddr initPay payList) =
    let
        (CFundingTxInfo txid vout val) = deriveMockFundingInfo cp
        openPath = mkOpenPath (cpSenderPubKey cp) (cpLockTime cp)
                mockChangeAddress initPay
    in
        PaySessionData
            (endPoint `mappend` cs openPath `mappend` "&test=true")
            (map (mappend endPoint . cs . mkPaymentPath txid (fromIntegral vout) Nothing) payList)








convertMockFundingInfo :: FundingTxInfo -> TxInfo
convertMockFundingInfo (CFundingTxInfo txid vout val)  =
    TxInfo txid 27 (OutInfo "" (fromIntegral val) (fromIntegral vout))

deriveMockFundingInfo :: ChannelParameters -> FundingTxInfo
deriveMockFundingInfo (CChannelParameters sendPK recvPK expTime) =
    CFundingTxInfo
        (HT.TxHash . fromJust . HC.bsToHash256 $ BS.take 32 $ cs . Bin.encode $ sendPK)
        1
        12345678900000


f = HC.makePrvKey

-- function createPaymentSignature(clientKeyPair,
--  fundingTxId, fundingVout,
-- redeemScript, changeAddress, changeAmount)