{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Util
(
    module Common.Util,
    module Common.URLParam,
    liftIO,
    mzero,
    unless, when,
    (<>),
    cs,
    view,
    HexBinEncode(..),
    HexBinDecode(..),
    JSON.ToJSON(toJSON),
    RecvPubKey(..)
)

where

import           Prelude hiding (userError)

import           Common.Types
import           Common.URLParam

import           Data.String.Conversions (cs)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (mzero)
import           Control.Lens.Getter (view)
import           Control.Monad (unless, when)
import           Data.Monoid ((<>))
import qualified Data.Aeson as JSON


-- New
import qualified Control.Monad.Error.Class as Except
import           Servant
import qualified Data.Serialize as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Network.Haskoin.Script as HS


class Bin.Serialize a => HexBinEncode a where
    hexEncode :: a -> BS.ByteString
    hexEncode = B16.encode . Bin.encode

class Bin.Serialize a => HexBinDecode a where
    hexDecode :: BS.ByteString -> Either String a
    hexDecode = Bin.decode . fst . B16.decode

instance HexBinEncode BS.ByteString where hexEncode = B16.encode
instance HexBinEncode RecvPubKey
instance HexBinEncode HS.Script
instance HexBinDecode BS.ByteString where hexDecode = Right . fst . B16.decode
instance HexBinDecode RecvPubKey
instance HexBinDecode HS.Script

--- HTTP error
userError' :: String -> AppM conf a
userError' = errorWithDescription 400

internalError :: String -> AppM conf a
internalError = errorWithDescription 400

onLeftThrow500 :: Either String a -> AppM conf a
onLeftThrow500   = either internalError return

errorWithDescription :: Int -> String -> AppM conf a
errorWithDescription code e = Except.throwError $
    err400 { errReasonPhrase = cs e, errBody = cs e, errHTTPCode = code}

