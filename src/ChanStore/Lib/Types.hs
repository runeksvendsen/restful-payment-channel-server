{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module ChanStore.Lib.Types
(
    module ChanStore.Lib.Types
  , MapItemResult(..)
)

where

import           Common.Types
import           Data.DiskMap (DiskMap, SyncAction,
                            CreateResult(..),
                            Serializable(..), ToFileName(..), Hashable(..), MapItemResult(..))

import           Data.Bitcoin.PaymentChannel.Movable
import           Data.Bitcoin.PaymentChannel.Util (deserEither)
import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as Bin
import           Data.String.Conversions (cs)
import           Control.Concurrent (ThreadId)
import qualified Servant.API.ContentTypes as Content
import           GHC.Generics

type Key = SendPubKey

data ChannelMap = ChannelMap
    (DiskMap Key ChanState)
    (Maybe (SyncAction,ThreadId))   -- Used when deferred sync is enabled

-- |Holds state for each payment channel
data ChanState =
    ReadyForPayment         MovableChan
  | ChannelSettled          HT.TxHash Payment (ReceiverPaymentChannel,BitcoinAmount)
  | SettlementInProgress    (ReceiverPaymentChannel,BitcoinAmount)

data UpdateResult =
    ChanClosed      TxHash BitcoinAmount
  | ChanBeingClosed
  | NoSuchChannel
        deriving Generic

data OpenRequest = OpenRequest ChannelParameters FundingTxInfo FullPayment deriving (Show, Generic)
data OpenResult =
    ChannelOpened   BitcoinAmount BitcoinAmount
  | OpenError       PayChanError
  | ChannelExists
        deriving Generic

data PayRequest = PayRequest Key FullPayment deriving Generic
data PayResult =
    PaymentReceived {
        paymentVal      ::  BitcoinAmount
      , chanValLeft     ::  BitcoinAmount
    }
  | PaymentError    PayChanError
  | PayUpdateError  UpdateResult
        deriving Generic

data CloseBeginRequest = CloseBeginRequest ChannelResource Signature deriving Generic
data CloseBeginResult  =
    CloseInitiated      (ReceiverPaymentChannel,BitcoinAmount)
  | IncorrectSig
  | CloseUpdateError    UpdateResult
        deriving Generic

instance Bin.Serialize UpdateResult
instance Bin.Serialize OpenRequest
instance Bin.Serialize OpenResult
instance Bin.Serialize PayRequest
instance Bin.Serialize PayResult
instance Bin.Serialize CloseBeginRequest
instance Bin.Serialize CloseBeginResult



-- Instances
instance Content.MimeUnrender Content.OctetStream UpdateResult where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream UpdateResult where
    mimeRender _ = BL.fromStrict . Bin.encode

instance Content.MimeUnrender Content.OctetStream CloseBeginResult where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream CloseBeginResult where
    mimeRender _ = BL.fromStrict . Bin.encode

instance Content.MimeUnrender Content.OctetStream CloseBeginRequest where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream CloseBeginRequest where
    mimeRender _ = BL.fromStrict . Bin.encode

instance Content.MimeUnrender Content.OctetStream OpenRequest where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream OpenRequest where
    mimeRender _ = BL.fromStrict . Bin.encode

instance Content.MimeUnrender Content.OctetStream OpenResult where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream OpenResult where
    mimeRender _ = BL.fromStrict . Bin.encode

instance Content.MimeUnrender Content.OctetStream PayRequest where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream PayRequest where
    mimeRender _ = BL.fromStrict . Bin.encode

instance Content.MimeUnrender Content.OctetStream PayResult where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream PayResult where
    mimeRender _ = BL.fromStrict . Bin.encode





instance ToFileName SendPubKey

instance Hashable SendPubKey where
    hashWithSalt salt sendPK =
        salt `hashWithSalt` serialize sendPK

instance Serializable SendPubKey where
    serialize   = Bin.encode
    deserialize = deserEither . cs

instance Serializable ChanState where
    serialize   = Bin.encode
    deserialize = deserEither . cs

instance Bin.Serialize ChanState where
    put (ReadyForPayment mc) =
        Bin.putWord8 0x02 >>
        Bin.put mc
    put (ChannelSettled txid payment s) =
        Bin.putWord8 0x03 >>
        Bin.put txid >> Bin.put payment >> Bin.put s
    put (SettlementInProgress s) =
        Bin.putWord8 0x04 >>
        Bin.put s

    get = Bin.getWord8 >>=
        (\byte -> case byte of
            0x02    -> ReadyForPayment   <$> Bin.get
            0x03    -> ChannelSettled   <$> Bin.get <*> Bin.get <*> Bin.get
            0x04    -> SettlementInProgress <$> Bin.get
            n       -> fail $ "unknown start byte: " ++ show n)




