{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module ChanStore.API where

import           ChanStore.Lib.Types
import           ChanStore.Orphans ()
import           Data.Bitcoin.PaymentChannel.Types -- (Payment, BitcoinAmount)
import qualified Network.Haskoin.Transaction as HT

import           Servant.API
import           Servant.Client
import           Data.Proxy

import           ConnManager.Types
import qualified ConnManager.Servant as Servant
import           Data.Time.Clock (UTCTime)
import           Control.Monad (void)



type RPC = ReceiverPaymentChannel


-- |The API exposed by this server.
type ChanStore =
       "store"  :> "by_id"                                        :> ReqBody '[OctetStream] RPC         :> Post '[OctetStream] CreateResult
  :<|> "store"  :> "by_id"                :> Capture "id" Key                                           :> Get  '[OctetStream] MaybeChanState
  :<|> "store"  :> "by_id"                :> Capture "id" Key     :> ReqBody '[OctetStream] Payment     :> Put  '[OctetStream] UpdateResult
  :<|> "settle" :> "begin"  :> "by_id"    :> Capture "id" Key                                           :> Put  '[OctetStream] RPC
  :<|> "settle" :> "begin"  :> "by_exp"   :> Capture "exp" UTCTime                                      :> Put  '[OctetStream] [RPC]
  :<|> "settle" :> "begin"  :> "by_value" :> Capture "val" BitcoinAmount                                :> Put  '[OctetStream] [RPC]
  :<|> "settle" :> "finish" :> "by_id"    :> Capture "id" Key     :> Capture "out" HT.TxHash            :> Post '[OctetStream] NoContent




mkChanStoreInterface :: ConnManager2 -> Interface
mkChanStoreInterface (Conn2 baseUrl man) =
    Interface
        (\rpc -> failOnLeft =<< Servant.runReq (chanAdd'            rpc man baseUrl) )
        (\k   -> failOnLeft =<< Servant.runReq (getMaybe <$> chanGet' k man baseUrl) )
        (\k p -> failOnLeft =<< Servant.runReq (chanUpdate'         k p man baseUrl) )
        (\exp -> failOnLeft =<< Servant.runReq (settleByExpBegin'   exp man baseUrl) )
        (\k   -> failOnLeft =<< Servant.runReq (settleByIdBegin'    k   man baseUrl) )
        (\val -> failOnLeft =<< Servant.runReq (settleByValBegin'   val man baseUrl) )
        (\k h -> failOnLeft =<< Servant.runReq (void $ settleFin'   k h man baseUrl) )
    where failOnLeft = either error return

data Interface = Interface {
    chanAdd             :: ReceiverPaymentChannel -> IO CreateResult
  , chanGet             :: Key -> IO (Maybe ChanState)
  , chanUpdate          :: Key -> Payment -> IO UpdateResult
  , settleByExpBegin    :: UTCTime -> IO [ReceiverPaymentChannel]
  , settleByIdBegin     :: Key -> IO ReceiverPaymentChannel
  , settleByValBegin    :: BitcoinAmount -> IO [ReceiverPaymentChannel]
  , settleFin           :: Key -> HT.TxHash -> IO ()
}

api :: Proxy ChanStore
api = Proxy


chanAdd' :<|> chanGet' :<|> chanUpdate' :<|> settleByIdBegin' :<|>
    settleByExpBegin' :<|> settleByValBegin' :<|> settleFin' =
        client api


