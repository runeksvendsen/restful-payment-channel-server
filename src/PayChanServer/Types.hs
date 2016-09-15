{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module PayChanServer.Types
(
    module PayChanServer.Types
  , module Common.Types
)
 where


import           Common.Types
import           Common.Util
import           PayChanServer.Config.Types
import           Data.Word                          (Word32)

-- Generated
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Data.Function ((&))


type Vout = Word32

type AppPC = AppM App


data ChannelStatus = ChannelOpen | ChannelClosed deriving (Show, Eq)
instance FromJSON ChannelStatus where
  parseJSON (String s) = case s of
    "open" -> return ChannelOpen
    "closed" -> return ChannelClosed
    e       -> fail $ "invalid channel status: " ++ cs e
  parseJSON _ = mzero
instance ToJSON ChannelStatus where
  toJSON ChannelOpen = String "open"
  toJSON ChannelClosed = String "closed"



-- Generated code with types modified:
-- |
data PaymentResult = PaymentResult
    { paymentResultChannelStatus    :: ChannelStatus    -- ^ Equal to \"open\" if the channel is still open, otherwise \"closed\". The channel is automatically closed when there is no value left to send. If a payment sends all remaining channel value to the server, the server will close the channel and set this field to \"closed\".
    , paymentResultChannelValueLeft :: BitcoinAmount    -- ^ Remaining channel value. This is the amount that the client/sender would receive if the channel was closed now.
    , paymentResultValueReceived    :: BitcoinAmount    -- ^ Value of the payment that was just received. This is the additional value assigned to the receiver/server with this payment.
    , paymentResultSettlementTxid   :: Maybe TxHash     -- ^ If channel_status equals \"closed\": the transaction ID of the Bitcoin transaction which settles the channel; otherwise null.
    } deriving (Show, Eq, Generic)


-- Just generated code
instance FromJSON PaymentResult where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "paymentResult")
instance ToJSON PaymentResult where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "paymentResult")

removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars = [("@", "'At"), ("!", "'Exclamation"), ("<=", "'Less_Than_Or_Equal_To"), ("#", "'Hash"), ("$", "'Dollar"), ("%", "'Percent"), ("&", "'Ampersand"), ("*", "'Star"), ("+", "'Plus"), ("-", "'Dash"), (".", "'Period"), (":", "'Colon"), ("|", "'Pipe"), ("<", "'LessThan"), ("!=", "'Not_Equal"), ("=", "'Equal"), ("^", "'Caret"), (">", "'GreaterThan"), ("_", "'Underscore"), (">=", "'Greater_Than_Or_Equal_To")]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer = if forParsing then flip T.replace else T.replace

-- |
data ChannelLocation = ChannelLocation
    { channelInfoChannelUri :: Text -- ^ The URL of the resource which must the POSTed to in order to open a new payment channel, after which further payments can be PUT on this resource. Close the payment channel by issuing a DELETE request on the resource.
    } deriving (Show, Eq, Generic)

instance FromJSON ChannelLocation where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "channelInfo")
instance ToJSON ChannelLocation where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "channelInfo")

-- |
data FundingInfo = FundingInfo
    { fundingInfoServerPubkey           :: Text -- ^ Server/value receiver public key. Hex-encoded, compressed Secp256k1 pubkey, 33 bytes.
    , fundingInfoDustLimit              :: Integer -- ^ (Satoshis) The server will not accept payments where the client change amount is less than this amount. This \"dust limit\" is necessary in order to avoid producing a settlement transaction that will not circulate in the Bitcoin P2P network because it contains an output of minuscule value. Consequently, the maximum amount, that can be sent over the payment channel, is the amount sent to the funding address minus this \"dust limit\".
    , fundingInfoFundingAddressCopy     :: Text -- ^ Server derived channel funding address. The client will confirm that its own derived funding address matches this one, before paying to it.
    , fundingInfoRedeemScriptCopy       :: Text -- ^ Server derived channel redeem script. Defines sender, receiver and channel expiration date. Used to construct the input in the payment transaction. The client will also verify that this matches what it expects. Hex-encoded.
    , fundingInfoOpenPrice              :: Integer -- ^ Price (in satoshis) for opening a channel with the given {exp_time}. This amount is paid in the initial channel payment when creating a new channel. May be zero, in which case a payment of zero value is transferred, ensuring that the channel can be closed at any time.
    , fundingInfoFundingTxMinConf       :: Int -- ^ Minimum confirmation count that the funding transaction must have before proceeding with opening a new channel.
    , fundingInfoSettlementPeriodHours  :: Int -- ^ The server reserves the right to close the payment channel this many hours before the specified expiration date. The server hasn't received any actual value until it publishes a payment transaction to the Bitcoin network, so it needs a window of time in which the client can no longer send payments over the channel, and yet the channel refund transaction hasn't become valid.
    , fundingInfoMinDurationHours       :: Int
    } deriving (Show, Eq, Generic)

instance FromJSON FundingInfo where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "fundingInfo")
instance ToJSON FundingInfo where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "fundingInfo")

-- | Contains information about a payment. A payment contains a signature over a hash of a Bitcoin transaction which redeems the outpoint pointed to by the resource&#39;s &#39;funding_txid&#39; and &#39;funding_vout&#39; parameters.
data PaymentData = PaymentData
    { paymentDataRedeemScript :: Text -- ^ The contract of the payment channel. Needed to generate the signature over the payment transaction.
    , paymentDataFundingTxid :: Text -- ^ The transaction ID of the Bitcoin transaction paying to the channel funding address.
    , paymentDataFundingVout :: Int -- ^ The output index/\"vout\" of the output (in the transaction) payingto the channel funding address.
    , paymentDataSignatureData :: Text -- ^ DER-encoded ECDSA signature (in hex). This is a SIGHASH_SINGLE|ANYONECANPAY signature over the the \"payment transaction\", which is a Bitcoin transaction that: redeems the outpoint specified by 'funding_txid' and 'funding_vout' using the redeem script defined in 'redeem_script', with an output which sends 'change_value' to 'change_address'.
    , paymentDataChangeValue :: Integer -- ^ The value sent back to the client in the payment transaction. The total amount transferred to the server is this amount subtracted from the value sent to the channel funding address.
    , paymentDataChangeAddress :: Text -- ^ The client change address as used in the only output of the payment transaction.
    } deriving (Show, Eq, Generic)

instance FromJSON PaymentData where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "paymentData")
instance ToJSON PaymentData where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "paymentData")