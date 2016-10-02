{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module ChanStore.API where

import           ChanStore.Lib.Types
import           ChanStore.Orphans ()
import qualified Network.Haskoin.Transaction as HT

import           Servant.API
import           Data.Time.Clock (UTCTime)



type RPC = ReceiverPaymentChannel


-- |The API exposed by this server.
type ChanStore =
  -- Storage
       "store"  :> "by_id"
            :> ReqBody '[OctetStream] OpenRequest           :> Post '[OctetStream] OpenResult

  :<|> "store"  :> "by_id"  :> Capture "client_pk" SendPubKey
            :> ReqBody '[OctetStream] FullPayment           :> Put  '[OctetStream] PayResult

  -- Management
  :<|> "manage" :> "get"  :> Capture "client_pk" SendPubKey :> Get  '[OctetStream] ChanInfoResult

  -- Settlement
  :<|> "settle" :> "begin"  :> "by_info"
            :> ReqBody '[OctetStream] CloseBeginRequest     :> Put  '[OctetStream] CloseBeginResult

  :<|> "settle" :> "begin"  :> "by_exp"   :> Capture "exp" UTCTime
                                                            :> Put  '[OctetStream] [RPC]

  :<|> "settle" :> "begin"  :> "by_value" :> Capture "val" BitcoinAmount
                                                            :> Put  '[OctetStream] [RPC]

  :<|> "settle" :> "finish" :> "by_id"    :> Capture "id"  Key    :> Capture "out" HT.TxHash
                                                            :> Post '[OctetStream] NoContent




