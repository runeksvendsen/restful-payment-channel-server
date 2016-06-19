{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Server.App (appInit)
import           Snap (serveSnaplet)
import           Snap.Http.Server (defaultConfig)

main :: IO ()
main = do
    serveSnaplet defaultConfig appInit

