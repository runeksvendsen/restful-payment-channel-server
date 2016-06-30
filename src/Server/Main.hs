{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Server.App (appInit)
import           Snap (Config, Snap, serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)
import           Snap.Http.Server.Config (setPort)

main :: IO ()
main = do
    maybePort <- return . maybe Nothing readMaybe =<< lookupEnv "PORT"

    let conf = case maybePort :: Maybe Word of
            Nothing     -> defaultConfig
            Just port   -> setPort (fromIntegral port) defaultConfig

    serveSnaplet conf appInit
