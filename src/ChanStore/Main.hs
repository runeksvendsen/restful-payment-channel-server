{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--Servant
{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, LambdaCase, TypeOperators #-}

module ChanStore.Main where

import           Prelude hiding (userError)

import qualified ChanStore.API as API
import           ChanStore.Lib.Types
import           ChanStore.Init             (init_chanMap, destroy_chanMap)
import           ChanStore.Lib.ChanMap
import           ChanStore.Lib.Settlement   (beginSettlingExpiringChannels, beginSettlingChannelsByValue,
                                            beginSettlingChannel, finishSettlingChannel)

import           PayChanServer.Main         (wrapArg)
import           PayChanServer.Config.Util  (configLookupOrFail, setBitcoinNetwork, getServerDBConf)
import           PayChanServer.Init (installHandlerKillThreadOnSig)

import           Common.URLParam (pathParamEncode)
import           Data.Bitcoin.PaymentChannel.Types (ReceiverPaymentChannel, BitcoinAmount, PaymentChannel(..), Payment)

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Catch (bracket)
import qualified Control.Monad.Error.Class as Except
import qualified Control.Monad.Reader as Reader
import           Control.Concurrent (myThreadId)

import qualified System.Posix.Signals as Sig
import           Data.Time.Clock (UTCTime)

import qualified Network.Haskoin.Transaction as HT
import           Data.String.Conversions    (cs)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Servant





api :: Proxy API.ChanStore
api = Proxy

type AppM = Reader.ReaderT ChannelMap Handler

serverEmbedMap :: ChannelMap -> Server API.ChanStore
serverEmbedMap map = enter (readerToEither map) server

readerToEither :: ChannelMap -> AppM :~> Handler
readerToEither cfg = Nat $ \x -> Reader.runReaderT x cfg

server :: ServerT API.ChanStore AppM
server = chanAdd :<|> chanGet :<|> chanUpdate :<|> settleByIdBegin :<|>
             settleByExpBegin :<|> settleByValBegin :<|> settleFin'
    where
        chanAdd rpc         = Reader.ask >>= create rpc
        chanGet k           = Reader.ask >>= get k
        chanUpdate k paym   = Reader.ask >>= update k paym
        settleByIdBegin  k  = Reader.ask >>= settleByKey k
        settleByExpBegin t  = Reader.ask >>= settleByExp t
        settleByValBegin v  = Reader.ask >>= settleByVal v
        settleFin' k h      = Reader.ask >>= settleFin k h


app :: ChannelMap -> Wai.Application
app map = serve api $ serverEmbedMap map

main :: IO ()
main = wrapArg $ \cfg _ -> do
    configLookupOrFail cfg "bitcoin.network" >>= setBitcoinNetwork
    port <- configLookupOrFail cfg "network.port" :: IO Word
    mainThread <- myThreadId
    _ <- installHandlerKillThreadOnSig Sig.sigTERM mainThread
    bracket
        (getServerDBConf cfg >>= init_chanMap)  -- 1. first do this
        destroy_chanMap                         -- 3. at the end always do this
        (Warp.run (fromIntegral port) . app)    -- 2. in the meantime do this



create :: ReceiverPaymentChannel -> ChannelMap -> AppM CreateResult
create newChanState map = do
    let key = getChannelID newChanState
    liftIO $ addChanState map key newChanState

get :: Key -> ChannelMap -> AppM MaybeChanState
get outPoint map = do
    maybeItem <- liftIO $ getChanState map outPoint
    return $ MaybeChanState maybeItem

update :: Key -> Payment -> ChannelMap -> AppM UpdateResult
update key payment map =
    liftIO (updateChanState map key payment) >>=
        (\exists -> case exists of
                ItemUpdated _ _  -> return WasUpdated
                NotUpdated       -> return WasNotUpdated
                NoSuchItem       -> errorWithDescription 404 "No such channel"
        )

settleByKey :: Key -> ChannelMap -> AppM ReceiverPaymentChannel
settleByKey key m =
    liftIO $ beginSettlingChannel m key

settleByExp :: UTCTime -> ChannelMap -> AppM [ReceiverPaymentChannel]
settleByExp settlementTimeCutoff m =
    liftIO $ beginSettlingExpiringChannels m settlementTimeCutoff

settleByVal :: BitcoinAmount -> ChannelMap -> AppM [ReceiverPaymentChannel]
settleByVal minValue m =
    liftIO $ beginSettlingChannelsByValue m minValue

settleFin :: Key -> HT.TxHash -> ChannelMap -> AppM NoContent
settleFin key settleTxId m = do
    res <- liftIO $ finishSettlingChannel m (key,settleTxId)
    case res of
        (ItemUpdated _ _) -> (liftIO . putStrLn)
            ("Settled channel " ++ cs (pathParamEncode key) ++
            " with settlement txid: " ++ cs (pathParamEncode settleTxId) ) >>
                return NoContent
        NotUpdated -> userError $ "Channel isn't in the process of being settled." ++
                                  " Did you begin settlement first?" ++
                                  " Also, are you sure you have the right key?"
        NoSuchItem -> errorWithDescription 404 "No such channel"




---- Util --
userError :: String -> AppM a
userError = errorWithDescription 400

onLeftThrow500 :: Either String a -> AppM a
onLeftThrow500   = either throw500 return
    where throw500 = errorWithDescription 500

errorWithDescription :: Int -> String -> AppM a
errorWithDescription code e = Except.throwError $
    err400 { errReasonPhrase = cs e, errBody = cs e, errHTTPCode = code}
