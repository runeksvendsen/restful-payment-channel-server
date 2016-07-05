{-# LANGUAGE  OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}


module Common.Types (
    ChannelStatus(..),
    ChanOpenResult (..),
    FundingInfo (..),
    PaymentWrapper (..),
    PaymentResult (..),
    ) where

import Data.Bitcoin.PaymentChannel.Types (BitcoinAmount)
import qualified Data.Bitcoin.PaymentChannel.Types as PayChan (Payment)

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Function ((&))
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import Data.String.Conversions (cs)

-- |
data ChanOpenResult = ChanOpenResult
    { chanOpenResultchannel_uri :: Text -- ^ The URI of the newly opened channel. Payments are PUT on this URI.
    , chanOpenResultpayment_result :: PaymentResult -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON ChanOpenResult where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "chanOpenResult")
instance ToJSON ChanOpenResult where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "chanOpenResult")

-- |
data FundingInfo = FundingInfo
    { fundingInfoserver_pubkey :: HC.PubKey -- ^ Server public key (33-byte, hex-encoded, compressed Secp256k1 pubkey)
    , fundingInfofunding_address_copy :: HC.Address -- ^ Payment channel funding address. Send bitcoins to this address to fund a new channel.
    , fundingInfoopen_price :: BitcoinAmount -- ^ Price (in satoshis) for opening a channel with the given {exp_time}. This amount is paid in the initial channel payment when creating a new channel. May be zero, in which case a payment of zero value is transferred, ensuring that the channel can be closed at any time.
    , fundingInfofunding_tx_min_conf :: Int -- ^ Minimum confirmation count that the funding transaction must have before proceeding with opening a new channel.
    , fundingInfosettlement_period_hours :: Int -- ^
    } deriving (Show, Eq, Generic)

instance FromJSON FundingInfo where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "fundingInfo")
instance ToJSON FundingInfo where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "fundingInfo")

-- |
data PaymentWrapper = PaymentWrapper
    { paymentpayment_data :: PayChan.Payment -- ^ Opaque payment data, base64-encoded
    } deriving (Show, Eq, Generic)

instance FromJSON PaymentWrapper where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "payment")
instance ToJSON PaymentWrapper where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "payment")

-- | Custom
data ChannelStatus = ChannelOpen | ChannelClosed deriving (Show, Eq)
instance FromJSON ChannelStatus where
  parseJSON (String s) = case s of
    "open" -> return ChannelOpen
    "closed" -> return ChannelClosed
    e       -> fail $ "invalid channel status: " ++ cs e
instance ToJSON ChannelStatus where
  toJSON ChannelOpen = String "open"
  toJSON ChannelClosed = String "closed"


-- |
data PaymentResult = PaymentResult
    { paymentResultchannel_status     :: ChannelStatus
    ,  paymentResultchannel_value_left :: BitcoinAmount -- ^ Remaining channel value. This is the amount that the client/sender would receive if the channel was closed now.
    , paymentResultvalue_received :: BitcoinAmount      -- ^ Value of the payment that was just received. This is the additional value assigned to the receiver/server with this payment.
    , paymentResultsettlement_txid :: Maybe HT.TxHash   -- ^ If channel_status equals "closed": the transaction ID of the Bitcoin transaction which settles the channel; otherwise null
    } deriving (Show, Eq, Generic)

instance FromJSON PaymentResult where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "paymentResult")
instance ToJSON PaymentResult where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "paymentResult")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars = [("@", "'At"), ("!", "'Exclamation"), ("#", "'Hash"), ("$", "'Dollar"), ("%", "'Percent"), ("&", "'Ampersand"), ("*", "'Star"), ("+", "'Plus"), ("-", "'Dash"), (":", "'Colon"), ("|", "'Pipe"), ("<", "'LessThan"), ("=", "'Equal"), ("^", "'Caret"), (">", "'GreaterThan")]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer = if forParsing then flip T.replace else T.replace


