{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module PayChanServer.API.Management where

import           PayChanServer.Types
import           Servant.API



-- | Internal PayChanServer management interface
type Man =
    -- Register a data payload which will be sent in the PaymentResult response
    -- to a successful payment of value greater than or equal to 'payment_value'.
       "channels" :>
            Capture "client_pubkey" SendPubKey :> "setup_payload" :> Capture "payment_value" BitcoinAmount
                :> ReqBody '[JSON] JSONString
                :> Verb 'POST 201 '[JSON] NoContent


