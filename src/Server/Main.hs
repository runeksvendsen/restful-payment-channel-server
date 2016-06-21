{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Server.App (appInit)
import           Snap (Config, Snap, serveSnaplet)
import           Snap.Snaplet (runSnaplet)
import           Snap.Http.Server (defaultConfig, httpServe)
import qualified Snap.Http.Server.Env as Env (httpServe)
-- Command line options
import Options.Applicative.Common
import Options.Applicative.Extra
import Options.Applicative.Builder


data CmdConfig = CmdConfig
  { useKeter :: Bool }

sample :: Parser CmdConfig
sample = CmdConfig
    <$> switch
      ( long "keter"
     <> help    ("Listen on port specified by \"PORT\" environment variable.\n" ++
                "Used by the \"keter\" deployment tool" ))

runWithConf :: CmdConfig -> IO ()
runWithConf (CmdConfig False) = serveSnapletWith httpServe
runWithConf (CmdConfig True) = serveSnapletWith Env.httpServe

serveSnapletWith :: (Config Snap a -> Snap () -> IO ()) -> IO ()
serveSnapletWith serveFunc = do
    (_,snapletRes,_) <- runSnaplet (Just "paychan") appInit
    serveFunc defaultConfig snapletRes


main = let
    opts = info (helper <*> sample)
            ( fullDesc
            <> progDesc "This is a small wrapper around the main app, for use with keter"
            <> header "RESTful Bitcoin payment channel server" )

        in execParser opts >>= runWithConf

