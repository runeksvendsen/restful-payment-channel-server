{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, LambdaCase, TypeOperators #-}

module  PayChanServer.App where

import           PayChanServer.Types
import           PayChanServer.Handler.FundingInfo (fundingInfoHandler)
import           PayChanServer.Handler.Open     (chanOpenHandler)
import           PayChanServer.Handler.Pay (chanPayHandler)
import           PayChanServer.Handler.Close (chanSettleHandler)
import           PayChanServer.Handler.BeginOpen (beginOpenHandler)

import           Servant
import qualified PayChanServer.API as API


server :: ServerT API.RBPCP AppPC
server = fundingInfo :<|> beginOpen :<|> chanOpen :<|> chanPay :<|> chanClose
    where
        fundingInfo = fundingInfoHandler
        beginOpen   = beginOpenHandler
        chanOpen    = chanOpenHandler
        chanPay     = chanPayHandler
        chanClose   = chanSettleHandler
