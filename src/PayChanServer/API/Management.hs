{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module PayChanServer.API.Management where

import           PayChanServer.Types
import           Servant.API



-- | Internal PayChanServer management interface
type Man =
    -- Get information about an open channel
    "channels" :> Capture "client_pubkey" SendPubKey :> "info" :> Get '[JSON] ChannelInfo


