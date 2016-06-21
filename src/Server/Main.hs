{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Server.App (appInit)
import           Snap (serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig)
import qualified Snap.Http.Server.Env as Env (httpServe)

main :: IO ()
main = do
    (_,snapletRes,_) <- runSnaplet (Just "paychan") appInit
    Env.httpServe defaultConfig snapletRes
--     serveSnaplet defaultConfig appInit

