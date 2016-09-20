{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module PayChanServer.API where

import           PayChanServer.Types
import           Servant.API
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Crypto      as HC
import           Data.Word (Word32)


type VER = "v2"
type BLT = BitcoinLockTime
type Sig = HC.Signature

type FundInfo  = VER :> "funding"  :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> "info" :> Get '[JSON] FundingInfo
type BeginOpen = VER :> "funding"  :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> "begin_open" :> Header "Host" String :> Get '[JSON] ChannelLocation
type ChanOpen  = VER :> "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" HT.TxHash :> Capture "funding_vout" Word32 :> ReqBody '[JSON] FullPayment :> Verb 'POST 201 '[JSON] PaymentResult
type ChanPay   = VER :> "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" HT.TxHash :> Capture "funding_vout" Word32 :> ReqBody '[JSON] FullPayment :> Put '[JSON] PaymentResult
type ChanClose = VER :> "channels" :> Capture "client_pubkey" SendPubKey :> Capture "exp_time" BLT :> Capture "funding_txid" HT.TxHash :> Capture "funding_vout" Word32 :> QueryParam "sig" Sig        :> Delete '[JSON] PaymentResult

type RBPCP =
       FundInfo
  :<|> BeginOpen
  :<|> ChanOpen
  :<|> ChanPay
  :<|> ChanClose

