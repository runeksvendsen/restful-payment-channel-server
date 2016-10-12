{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--Servant
{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, LambdaCase, TypeOperators #-}

module ChanStore.Main where

import           Prelude hiding (userError)

import           Common.Types
import           Common.Util

import qualified ChanStore.API as API
import           ChanStore.Lib.Types
import           ChanStore.Init             (init_chanMap)
import           ChanStore.Lib.Store
import           ChanStore.Lib.Settlement   (beginSettlingExpiringChannels, beginSettlingChannelsByValue,
                                            beginSettlingChannel, finishSettlingChannel)

import           PayChanServer.Main         (wrapArg)
import           PayChanServer.Config.Util  (configLookupOrFail, setBitcoinNetwork, getServerDBConf)
import           PayChanServer.Init (installHandlerKillThreadOnSig)



import           Control.Monad.Catch (bracket)
import qualified Control.Monad.Reader as Reader
import           Control.Concurrent (myThreadId)

import qualified System.Posix.Signals as Sig
import           Data.Time.Clock (UTCTime)

import qualified Network.Haskoin.Transaction as HT

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant

-- Profile
import           Test.Profile (profile_selfDestruct)



type AppCS = AppM ChannelMap

api :: Proxy API.ChanStore
api = Proxy

serverEmbedMap :: ChannelMap -> Server API.ChanStore
serverEmbedMap map = enter (readerToEither map) server

readerToEither :: ChannelMap -> AppCS :~> Handler
readerToEither cfg = Nat $ \x -> Reader.runReaderT x cfg

server :: ServerT API.ChanStore AppCS
server = chanOpen :<|> chanPay :<|> manGetChan :<|> settleByInfoBegin :<|>
             settleByExpBegin :<|> settleByValBegin :<|> settleFin'
    where
        chanOpen or           = Reader.ask >>= open or
        chanPay k paym        = Reader.ask >>= pay k paym
        manGetChan k          = Reader.ask >>= get k
        settleByInfoBegin req = Reader.ask >>= settleByInfo req
        settleByExpBegin t    = Reader.ask >>= settleByExp t
        settleByValBegin v    = Reader.ask >>= settleByVal v
        settleFin' k h        = Reader.ask >>= settleFin k h


app :: ChannelMap -> Wai.Application
app map = serve api $ serverEmbedMap map

main :: IO ()
main = wrapArg $ \cfg _ -> do
    configLookupOrFail cfg "bitcoin.network" >>= setBitcoinNetwork
    port <- configLookupOrFail cfg "network.port" :: IO Word
    mainThread <- myThreadId
    _ <- installHandlerKillThreadOnSig Sig.sigTERM mainThread
--     myThreadId >>= profile_selfDestruct
    bracket
        (getServerDBConf cfg >>= init_chanMap)  -- 1. first do this
        destroy_chanMap                         -- 3. at the end always do this
        (Warp.run (fromIntegral port) . app)    -- 2. in the meantime do this
    where destroy_chanMap = return



open :: OpenRequest -> ChannelMap -> AppCS OpenResult
open openReq map = liftIO $ addChanState map openReq

pay :: Key -> FullPayment -> ChannelMap -> AppCS PayResult
pay key payment map = liftIO $ registerPayment map (PayRequest key payment)

get :: Key -> ChannelMap -> AppCS ChanInfoResult
get key map = liftIO $ getChannelInfo map key


settleByInfo :: CloseBeginRequest -> ChannelMap -> AppCS CloseBeginResult
settleByInfo req m =
    liftIO $ closeBegin m req

settleByExp :: UTCTime -> ChannelMap -> AppCS [ReceiverPaymentChannel]
settleByExp settlementTimeCutoff m =
    liftIO $ map fst <$> beginSettlingExpiringChannels m settlementTimeCutoff

settleByVal :: BitcoinAmount -> ChannelMap -> AppCS [ReceiverPaymentChannel]
settleByVal minValue m =
    liftIO $ map fst <$> beginSettlingChannelsByValue m minValue

settleFin :: Key -> HT.TxHash -> ChannelMap -> AppCS NoContent
settleFin key settleTxId m = do
    res <- liftIO $ finishSettlingChannel m (key,settleTxId)
    case res of
        ItemUpdated{} -> (liftIO . putStrLn)
            ("Settled channel with client pubkey " ++ cs (pathParamEncode key) ++
            ", settlement txid: " ++ cs (pathParamEncode settleTxId) ) >>
                return NoContent
        NotUpdated{} -> userError' $ "Channel isn't in the process of being settled." ++
                                  " Did you begin settlement first?" ++
                                  " Also, are you sure you have the right key?"
        NoSuchItem -> errorWithDescription 404 "No such channel"




---- Util --

