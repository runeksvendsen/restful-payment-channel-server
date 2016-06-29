{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Server.App (appInit)
import           Snap (Config, Snap, serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)
import qualified Snap.Http.Server.Env as Env (httpServe)


main = serveSnaplet defaultConfig appInit
