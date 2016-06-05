{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Server.App (appInit)
import           Snap (serveSnaplet)
import           Snap.Http.Server (defaultConfig)
import qualified Network.Haskoin.Constants as HCC

main :: IO ()
main = do
    HCC.switchToTestnet3
    serveSnaplet defaultConfig appInit

