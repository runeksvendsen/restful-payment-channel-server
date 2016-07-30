{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Common where

import           Common.URLParam

import           Data.Bitcoin.PaymentChannel
import           Data.Bitcoin.PaymentChannel.Types (BitcoinAmount, Payment, ChannelParameters(..),
                                                    SendPubKey, RecvPubKey, b64Encode,
                                                    IsPubKey(getPubKey))

import           Data.Aeson
    (Result(..), Value(Number, Object, String), FromJSON, ToJSON, parseJSON, toJSON,
    fromJSON, withScientific, eitherDecodeStrict, encode, decode, (.=), (.:), object)

import           Data.Bitcoin.PaymentChannel.Util
import           Data.Bitcoin.PaymentChannel.Types (RecvPubKey(..), IsPubKey(getPubKey))

import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Util as HU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
import           Data.Word (Word32)
import           Data.EitherR (fmapL)
import           Data.String.Conversions (cs)
import           Text.Printf (printf)
import qualified Data.Binary as Bin
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)


