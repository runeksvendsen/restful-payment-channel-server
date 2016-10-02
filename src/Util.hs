{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, LambdaCase, TypeOperators #-}

module Util
(
    module Util.Hex
  , cs
  , (<>)
  , readerToEither
  , envReadPort
  , fmapL
  , errorWithDescription
  , runLocalhost
)
where

import           Util.Hex
import           Types
import           Data.String.Conversions (cs)
import           Data.Monoid ((<>))
import           Servant
import           Data.EitherR (fmapL)

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Error.Class as Except

import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- |Transform an 'AppM conf' into a 'Servant.Handler'
readerToEither :: conf -> AppM conf :~> Handler
readerToEither cfg = Nat $ \x -> Reader.runReaderT x cfg

envReadPort :: IO (Maybe Word)
envReadPort = maybe Nothing readMaybe <$> lookupEnv "PORT"

errorWithDescription :: Int -> String -> AppM conf a
errorWithDescription code e = Except.throwError $
    err400 { errReasonPhrase = cs e, errBody = cs e, errHTTPCode = code}

runLocalhost :: Word -> Wai.Application -> IO ()
runLocalhost port = Warp.runSettings settings
    where settings = Warp.setPort (fromIntegral port) $
                        Warp.setHost "127.0.0.1" Warp.defaultSettings
