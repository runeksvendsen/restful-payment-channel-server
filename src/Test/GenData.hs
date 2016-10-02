{-# LANGUAGE  OverloadedStrings #-}

module Test.GenData where

import           Common.Util
import           Common.Types
import qualified PayChanServer.URI as URI

import           Data.Bitcoin.PaymentChannel (channelWithInitialPaymentOf, sendPayment)
import           Data.Bitcoin.PaymentChannel.Util (fpGetSig, toWord32, parseBitcoinLocktime, getFundingAddress)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize as Bin
import           Data.Aeson         (object, (.=), Value(Object), parseJSON, FromJSON, (.:))
import qualified Data.Text as T
import System.Entropy (getEntropy)
import Crypto.Secp256k1 (secKey)
import Data.Time.Clock.POSIX (getPOSIXTime)
import BlockchainAPI.Types (TxInfo(..))


mockChangeAddress = "mmA2XECa7bVERzQKkyy1pNBQ3PC4HnxTC5"
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
    csPayments    :: [FullPayment]
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
                (`HC.signMsg` privClient) (getFundingAddress cp) 100000
        payStateList = tail $ iterate iterateFunc (amt,initPay,initState)
        payList = map getPayment payStateList
        getPayment (_,p,_) = p
    in
        ChannelSession endPoint cp (getFundingAddress cp) (initPay : take numPayments payList)
--------

data PaySessionData = PaySessionData {
    resourceURI :: T.Text,
    closeURI    :: T.Text,
    payDataList :: [FullPayment]
}

instance ToJSON PaySessionData where
    toJSON (PaySessionData openURL closeURL payList) =
        object [
            "open_url"      .= openURL,
            "close_url"     .= closeURL,
            "payment_data"  .= payList
         ]

instance FromJSON PaySessionData where
    parseJSON (Object v) = PaySessionData <$>
                v .: "open_url" <*> v .: "close_url" <*> v .: "payment_data"
    parseJSON _ = mzero

-- mkCloseURI
getSessionData :: ChannelSession -> PaySessionData
getSessionData (ChannelSession endPoint cp@(CChannelParameters sendPK _ lt _) _ payList) =
    let
        (CFundingTxInfo txid vout _) = deriveMockFundingInfo cp
        openURL  = cs . show $ URI.mkChanURI sendPK lt txid vout
        closeURL = cs . show $ URI.mkCloseURI sendPK lt txid vout -- (last payList)
        fullURL resource = "http://" <> endPoint <> "/" <> resource
    in
        PaySessionData (fullURL openURL) (fullURL closeURL) payList


-- | Deprecated
convertMockFundingInfo :: FundingTxInfo -> TxInfo
convertMockFundingInfo = TxInfo 27

deriveMockFundingInfo :: ChannelParameters -> FundingTxInfo
deriveMockFundingInfo (CChannelParameters sendPK _ expTime _) =
    CFundingTxInfo
        (HT.TxHash $ HC.hash256 $ cs . Bin.encode $ sendPK)
        (toWord32 expTime `mod` 7)
        12345678900000

