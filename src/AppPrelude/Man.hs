{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module AppPrelude.Man
(
    module AppPrelude.Man
  , module AppPrelude.Types
--  , MapItemResult(..)
)

where

import           AppPrelude.Types
import qualified RBPCP.Types as RBPCP
--import           Data.DiskMap (DiskMap,
--                            CreateResult(..),
--                            Serializable(..), ToFileName(..), Hashable(..), MapItemResult(..))

import           PaymentChannel
import           PaymentChannel.Types
import           PaymentChannel.Util (deserEither)
import qualified Network.Haskoin.Transaction as HT
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as Bin
import           Data.String.Conversions (cs)
import qualified Servant.API.ContentTypes as Content
import           GHC.Generics
import           Data.Serialize.Text ()


-- |Management
data ChanInfoResult =
    OK ServerPayChanX RBPCP.ChannelStatus (Maybe HT.TxHash)
  | ChanNotFound
        deriving Generic



instance Bin.Serialize ChanInfoResult



instance Content.MimeUnrender Content.OctetStream ChanInfoResult where
    mimeUnrender _ = deserEither . BL.toStrict
instance Content.MimeRender Content.OctetStream ChanInfoResult where
    mimeRender _ = BL.fromStrict . Bin.encode



