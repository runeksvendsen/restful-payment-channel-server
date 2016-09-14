{-# LANGUAGE  OverloadedStrings #-}

module Test.GenData where

import           Common.ResourceURL (channelOpenURL,mkOpenQueryParams,mkPaymentURL)
import           Common.URLParam   (pathParamDecode)

import           Data.Bitcoin.PaymentChannel (channelWithInitialPaymentOf, sendPayment)
import           Data.Bitcoin.PaymentChannel.Util (BitcoinLockTime(..), toWord32, parseBitcoinLocktime,
                                                   getFundingAddress)
import           Data.Bitcoin.PaymentChannel.Types
    (Payment, ChannelParameters(..), FundingTxInfo(..), SendPubKey(..), RecvPubKey(..), IsPubKey(getPubKey))
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize as Bin
import           Data.Aeson         (object, ToJSON, toJSON, (.=), encode,
                                     Value(Object), parseJSON, FromJSON, (.:))
import Control.Monad (mzero)

import Data.String.Conversions (cs)
import qualified Data.Text as T
import System.Entropy (getEntropy)
import Crypto.Secp256k1 (secKey)
import Data.Time.Clock.POSIX (getPOSIXTime)
-- import Server.Config (pubKeyServer, fundsDestAddr, openPrice)
import BlockchainAPI.Types (TxInfo(..))

mockChangeAddress = "mmA2XECa7bVERzQKkyy1pNBQ3PC4HnxTC5"
nUM_PAYMENTS = 100 :: Int
cHAN_DURATION = 3600 * 24 * 7 :: Integer



genData :: T.Text -> Int -> String -> IO PaySessionData
genData endpoint numPayments pubKeyServerStr = do
    pubKeyServer <- either (const $ fail "failed to parse server pubkey") return
        (pathParamDecode $ cs pubKeyServerStr)
    -- Generate fresh private key
    privSeed <- getEntropy 32
    prvKey <- maybe (fail "couldn't derive secret key from seed") return $
            fmap HC.makePrvKey (secKey privSeed)
    expTime <- fmap round getPOSIXTime
    let sess = genChannelSession endpoint numPayments prvKey pubKeyServer
            (parseBitcoinLocktime $ fromIntegral $ expTime + cHAN_DURATION)

    return $ getSessionData sess



data ChannelSession = ChannelSession {
    csEndPoint    :: T.Text,
    csParams      :: ChannelParameters,
    csFundAddr    :: HC.Address,
    csInitPayment :: Payment,
    csPayments    :: [Payment]
}

-- | Generate payment of value 1 satoshi
iterateFunc (_,_,s) = sendPayment s 1

genChannelSession :: T.Text -> Int -> HC.PrvKey -> HC.PubKey -> BitcoinLockTime -> ChannelSession
genChannelSession endPoint numPayments privClient pubServer expTime =
    let
        cp = CChannelParameters
                (MkSendPubKey $ HC.derivePubKey privClient) (MkRecvPubKey pubServer)
                expTime
                0
        fundInfo = deriveMockFundingInfo cp
        (amt,initPay,initState) = channelWithInitialPaymentOf cp fundInfo
                (`HC.signMsg` privClient) mockChangeAddress 100000
        payStateList = tail $ iterate iterateFunc (amt,initPay,initState)
        payList = map getPayment payStateList
        getPayment (_,p,_) = p
    in
        ChannelSession endPoint cp (getFundingAddress cp) initPay (take numPayments payList)
--------

data PaySessionData = PaySessionData {
    openURL     :: T.Text,
    payURLList  :: [T.Text],
    closeURL    :: T.Text
}

instance ToJSON PaySessionData where
    toJSON (PaySessionData openURL payURLs closeURL) =
        object [
            "open_url"      .= openURL,
            "payment_urls"  .= payURLs,
             "close_url"    .= closeURL
         ]

instance FromJSON PaySessionData where
    parseJSON (Object v) = PaySessionData <$>
                v .: "open_url" <*> v .: "payment_urls" <*> v .: "close_url"
    parseJSON _ = mzero


getSessionData :: ChannelSession -> PaySessionData
getSessionData (ChannelSession endPoint cp _ initPay payList) =
    let
        (CFundingTxInfo txid vout _) = deriveMockFundingInfo cp
        chanId = HT.OutPoint txid (fromIntegral vout)
        openURL = channelOpenURL False (cs endPoint) "/v1" (cpSenderPubKey cp)
                (cpLockTime cp) ++ mkOpenQueryParams mockChangeAddress initPay
        payURLs = map (cs . mkPaymentURL False (cs endPoint) "/v1" chanId) payList
    in
        PaySessionData
            (cs openURL)
            payURLs
            (last payURLs)



-- | Deprecated
convertMockFundingInfo :: FundingTxInfo -> TxInfo
convertMockFundingInfo = TxInfo 27

deriveMockFundingInfo :: ChannelParameters -> FundingTxInfo
deriveMockFundingInfo (CChannelParameters sendPK _ expTime _) =
    CFundingTxInfo
        (HT.TxHash $ HC.hash256 $ cs . Bin.encode $ sendPK)
        (toWord32 expTime `mod` 7)
        12345678900000

