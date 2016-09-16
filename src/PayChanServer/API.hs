{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module PayChanServer.API where

import           PayChanServer.Types
import           Servant.API
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto      as HC
import           Data.Word (Word32)


type BLT = BitcoinLockTime
type Sig = HC.Signature

type FundInfo  = "funding"  :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> "info" :> Verb 'GET 200 '[JSON] FundingInfo
type BeginOpen = "funding"  :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> "begin_open" :> Header "Host" String :> Verb 'GET 200 '[JSON] ChannelLocation
type ChanOpen  = "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" HT.TxHash :> Capture "funding_vout" Word32 :> ReqBody '[JSON] FullPayment :> Verb 'POST 201 '[JSON] PaymentResult
type ChanPay   = "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" HT.TxHash :> Capture "funding_vout" Word32 :> ReqBody '[JSON] FullPayment :> Verb 'PUT 200 '[JSON] PaymentResult
type ChanClose = "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" HT.TxHash :> Capture "funding_vout" Word32 :> QueryParam "sig" Sig        :> Verb 'DELETE 200 '[JSON] PaymentResult

type RBPCP =
       FundInfo
  :<|> BeginOpen
  :<|> ChanOpen
  :<|> ChanPay
  :<|> ChanClose

