{-# LANGUAGE FlexibleInstances #-}
module ChanStore.Lib.Store where


import           Data.DiskMap (newDiskMap,
                            addItem, getItem,
                            CreateResult(..),
                            MapItemResult(..),getResult,
                            mapGetItem, updateIfRight,
                            getItemCount,
                            getFilteredKeys,
                            makeReadOnly)

import           Common.Types
import           ChanStore.Lib.Types
import           PayChanServer.Config.Types (ServerDBConf(..))
import           PayChanServer.Types (ChannelInfo(..), ChannelStatus(..))
import           Data.Bitcoin.PaymentChannel.Types
import           Data.Bitcoin.PaymentChannel.Movable

import           Control.Concurrent.STM (STM, atomically)
import           Control.Monad.Catch (finally, handleAll, Exception)
import           Control.Concurrent (forkIO, killThread, threadDelay)

import           Data.Maybe (fromMaybe)


-- Storage interface
addChanState :: ChannelMap -> OpenRequest -> IO OpenResult
addChanState (ChannelMap diskMap) (OpenRequest cp fti fp) =
    either
        (return . OpenError)
        (\(amt, mc, valLeft) ->
            addItem diskMap key (ReadyForPayment mc) >>=
            (\res -> case res of
                    Created         -> return $ ChannelOpened amt valLeft
                    AlreadyExists   -> return   ChannelExists))
        (newMovableChan cp fti fp)
    where key = cpSenderPubKey cp

registerPayment :: ChannelMap -> PayRequest -> IO PayResult
registerPayment (ChannelMap diskMap) (PayRequest key fp) =
    let
        updFunc :: ChanState -> Either PayResult (ChanState, PayResult)
        -- Success
        updFunc (ReadyForPayment mc)   =
            case recvSinglePayment mc fp of
                Right (valRecvd, newMC, valLeft) ->
                        Right   ( ReadyForPayment newMC
                                , PaymentReceived valRecvd valLeft (getFundingAmount mc)
                                )
                Left payChanErr    ->
                        Left $ PaymentError payChanErr
        -- Error
        updFunc (ChannelSettled txid _ (rpc,_)) =
            Left $ PayUpdateError $ ChanClosed txid (channelValueLeft rpc)
        updFunc SettlementInProgress{} = Left $ PayUpdateError ChanBeingClosed
        checkMapUpdRes res             =
            fromMaybe (PayUpdateError NoSuchChannel) (getResult res)
    in
        checkMapUpdRes <$> updateIfRight diskMap key updFunc

closeBegin :: ChannelMap -> CloseBeginRequest -> IO CloseBeginResult
closeBegin (ChannelMap diskMap) (CloseBeginRequest (ChannelResource clientPK lt op) fp) =
    let
        updFunc :: ChanState -> Either CloseBeginResult (ChanState, CloseBeginResult)
        updFunc (ReadyForPayment mc)   =
            case recvClosingPayment mc fp of
                Right (rpc,v) -> Right ( SettlementInProgress (rpc,v) , CloseInitiated (rpc,v) )
                Left e        -> Left  $ ClosingPaymentError e
        updFunc (ChannelSettled txid _ (rpc,_)) =
            Left $ CloseUpdateError $ ChanClosed txid (channelValueLeft rpc)
        updFunc SettlementInProgress{} = Left $ CloseUpdateError ChanBeingClosed
        checkMapUpdRes res             = fromMaybe (CloseUpdateError NoSuchChannel) (getResult res)
    in
        checkMapUpdRes <$> updateIfRight diskMap clientPK updFunc


-- |Management
getChannelInfo :: ChannelMap -> SendPubKey -> IO ChanInfoResult
getChannelInfo (ChannelMap diskMap) clientPK = do
    maybeItem <- getItem diskMap clientPK
    case maybeItem of
        Nothing -> return ChanNotFound
        Just cs -> return $ handleChanState cs
    where handleChanState cs =
            case cs of
                ReadyForPayment mc            ->
                    OK (fst $ getCurrentState mc)   ChannelOpen Nothing
                ChannelSettled txId _ (rpc,_) ->
                    OK rpc                          ChannelClosed (Just txId)
                SettlementInProgress (rpc,_)  ->
                    OK rpc                          ChannelClosed Nothing


-- data ChannelInfo = ChannelInfo
--   { sender          :: SendPubKey
--   , status          :: ChannelStatus
--   , value_left      :: BitcoinAmount
--   , expires         :: BitcoinLockTime
--   , funding_value   :: BitcoinAmount
--   , funding_address :: Address
--   , funding_source  :: OutPoint
--   , settlement_txid :: Maybe TxHash
--   }
