{-# LANGUAGE OverloadedStrings, DeriveFunctor, GeneralizedNewtypeDeriving, RankNTypes #-}

module Test.GenData where

import           AppPrelude.Util
import           AppPrelude.Types
import qualified RBPCP.Types as RBPCP
import qualified PayChanServer.URI as URI

import           PaymentChannel
import           PaymentChannel.Util (toWord32, parseLockTime, getFundingAddress)

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Data.Serialize as Bin
import           Data.Aeson         (object, (.=), Value(Object), parseJSON, FromJSON, (.:))
import qualified Data.Text as T
import System.Entropy             (getEntropy)
import Crypto.Secp256k1           (secKey)
import Data.Time.Clock.POSIX      (getPOSIXTime)
--import Control.Monad.Time
import Data.Functor.Identity


mockChangeAddress = "mmA2XECa7bVERzQKkyy1pNBQ3PC4HnxTC5"
cHAN_DURATION = 3600 * 24 * 7 :: Integer



genData :: T.Text -> Int -> String -> IO PaySessionData
genData endpoint numPayments pubKeyServerStr = do
    pubKeyServer <- either (const $ fail "failed to parse server pubkey") return
        (pathParamDecode $ cs pubKeyServerStr)
    -- Generate fresh private key
    privSeed <- getEntropy 32
    prvKey <- maybe (fail "couldn't derive secret key from seed") return $
            fmap HC.makePrvKeyC (secKey privSeed)
    expTime <- fmap round getPOSIXTime
    let sess = genChannelSession endpoint numPayments prvKey pubKeyServer
            (fromMaybe (error "Bad LockTimeDate") $
                parseLockTime $ fromIntegral $ expTime + cHAN_DURATION)

    return $ getSessionData sess



data ChannelSession = ChannelSession {
    csEndPoint    :: T.Text,
    csParams      :: ChanParams,
    csFundAddr    :: HC.Address,
    csPayments    :: [SignedPayment]
}


-- | Generate payment of value 1 satoshi
iterateFunc :: Monad m => (ClientPayChanI BtcSig, t) -> m (Either BtcError (ClientPayChanI BtcSig, SignedPayment))
iterateFunc (s,_) = createPayment s 1

getOrFail :: forall t a. Show a => Identity (Either a t) -> t
getOrFail (Identity (Right a)) = a
getOrFail (Identity (Left e))  = error $ "genChannelSession: " ++ show e

genChannelSession :: T.Text -> Int -> HC.PrvKeyC -> HC.PubKeyC -> LockTimeDate -> ChannelSession
genChannelSession endPoint numPayments privClient pubServer expTime =
    let
        cp = MkChanParams
                (MkSendPubKey $ HC.derivePubKey privClient) (MkRecvPubKey pubServer)
                expTime

        fundInfo = deriveMockFundingInfo cp

        (initState,initPay) = getOrFail $ channelWithInitialPayment privClient cp fundInfo
                (getFundingAddress cp) 100000
        payStateList = tail $ iterate (getOrFail . iterateFunc) (initState,initPay)
        payList = map getPayment payStateList
        getPayment (_,p) = p
    in
        ChannelSession endPoint cp (getFundingAddress cp) (initPay : take numPayments payList)
--------

data PaySessionData = PaySessionData {
    resourceURI :: T.Text,
    closeURI    :: T.Text,
    payDataList :: [RBPCP.Payment]
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
    parseJSON _ = mempty

-- mkCloseURI
getSessionData :: ChannelSession -> PaySessionData
getSessionData (ChannelSession endPoint cp@(MkChanParams sendPK _ lt) _ payList) =
    let
        (CFundingTxInfo txid vout _) = deriveMockFundingInfo cp
        openURL  = cs . show $ URI.mkChanURI (conv sendPK) (toWord32 lt) txid vout
        closeURL = cs . show $ URI.mkCloseURI (conv sendPK) (toWord32 lt) txid vout -- (last payList)
        fullURL resource = "http://" <> endPoint <> "/" <> resource
        conv = RBPCP.Client . getPubKey
    in
        PaySessionData (fullURL openURL) (fullURL closeURL)
                       (map ((`RBPCP.Payment` "") . toPaymentData) payList)


deriveMockFundingInfo :: ChanParams -> FundingTxInfo
deriveMockFundingInfo (MkChanParams sendPK _ expTime) =
    CFundingTxInfo
        (HT.TxHash $ HC.hash256 $ cs . Bin.encode $ sendPK)
        (toWord32 expTime `mod` 7)
        (either (error "Dusty bitcoins") id $ mkNonDusty 12345678900000)

