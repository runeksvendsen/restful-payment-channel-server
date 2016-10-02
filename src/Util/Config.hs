module Util.Config
(
    wrapArg
  , loadConfig
  , configLookupOrFail
)
 where

import           Util
import qualified Data.Configurator.Types as Configurator
import           System.Environment (getArgs, getProgName)
import qualified Data.Configurator as Conf

loadConfig :: String -> IO Configurator.Config
loadConfig confFile = Conf.load [Conf.Required confFile]

-- |Load config file data from file given as the first argument
wrapArg :: (Configurator.Config -> String -> IO ()) -> IO ()
wrapArg main' = do
    args <- getArgs
    prog <- getProgName
    if  length args < 1 then
            putStrLn $ "Usage: " ++ prog ++ " /path/to/config.cfg"
        else do
            let cfgFile = head args
            putStrLn $ "Using config file " ++ show cfgFile
            cfg <- loadConfig cfgFile
            main' cfg cfgFile


configLookupOrFail :: Configurator.Configured a =>
    Configurator.Config
    -> Configurator.Name
    -> IO a
configLookupOrFail conf name =
    Conf.lookup conf name >>= maybe
        (fail $ "ERROR: Failed to read key \"" ++ cs name ++
            "\" in config (key not present or invalid)")
        return

