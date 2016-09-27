{-|
Module      : Lib.Interface
Description : ChannelStore client interface
Copyright   : Rune K. Svendsen
License     : PublicDomain
Maintainer  : runesvend@gmail.com
Stability   : experimental
Portability : POSIX

Potato
-}

module ChanStore.Interface
(
    Interface(chanOpen , chanPay
            , manPayData
            , settleByExpBegin
            , settleByInfoBegin , settleFin)
  , mkChanStoreInterface
  , ConnManager
  , ChanState(..)
)
where

import           ChanStore.Lib.Types
import qualified ChanStore.API as API
import           ConnManager.Types

import           Data.Bitcoin.PaymentChannel.Types
import           Data.Time.Clock (UTCTime)
import qualified Network.Haskoin.Transaction as HT
import           Servant
import           Servant.Client
import qualified ConnManager.Servant as Servant
import           Control.Monad (void)

mkChanStoreInterface :: ConnManager2 -> Interface
mkChanStoreInterface (Conn2 baseUrl man) =
    Interface
        (\or  -> failOnLeft =<< Servant.runReq (chanOpen'           or  man baseUrl) )
        (\k fp-> failOnLeft =<< Servant.runReq (chanPay'           k fp man baseUrl) )
        (\k a s-> failOnLeft =<< Servant.runReq (manPayData'        k a s man baseUrl) )
        (\exp -> failOnLeft =<< Servant.runReq (settleByExpBegin'   exp man baseUrl) )
        (\inf -> failOnLeft =<< Servant.runReq (settleByInfoBegin'  inf man baseUrl) )
        (\val -> failOnLeft =<< Servant.runReq (settleByValBegin'   val man baseUrl) )
        (\k h -> failOnLeft =<< Servant.runReq (void $ settleFin'   k h man baseUrl) )
    where failOnLeft = either error return

data Interface = Interface {
    chanOpen            :: OpenRequest                  -> IO OpenResult
  , chanPay             :: Key -> FullPayment           -> IO PayResult
  , manPayData          :: Key -> BitcoinAmount -> JSONString -> IO DataPayloadResult
  , settleByExpBegin    :: UTCTime                      -> IO [ReceiverPaymentChannel]
  , settleByInfoBegin   :: CloseBeginRequest            -> IO CloseBeginResult
  , settleByValBegin    :: BitcoinAmount                -> IO [ReceiverPaymentChannel]
  , settleFin           :: Key -> HT.TxHash             -> IO ()
}

api :: Proxy API.ChanStore
api = Proxy

chanOpen' :<|> chanPay' :<|> manPayData' :<|> settleByInfoBegin' :<|>
    settleByExpBegin' :<|> settleByValBegin' :<|> settleFin' =
        client api

