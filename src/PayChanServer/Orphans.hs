module PayChanServer.Orphans where

import Common.Types
import Common.Util

import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), withText, genericToJSON, genericParseJSON)


instance ToJSON Script where
    toJSON = String . cs . hexEncode

instance FromJSON Script where
    parseJSON = withText "BitcoinScript" (either fail return . hexDecode . cs)
