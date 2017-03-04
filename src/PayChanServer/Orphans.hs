module PayChanServer.Orphans where

import AppPrelude.Types
import AppPrelude.Util

import Data.Aeson (Value(..), FromJSON(..), ToJSON(..), withText, genericToJSON, genericParseJSON)


--instance ToJSON S Text "BitcoinScript" (either fail return . hexDecode . cs)
