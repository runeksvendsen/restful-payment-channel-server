module Types.Config where

import qualified Data.Configurator.Types as Configurator

class FromConfig a where
    fromConf :: Configurator.Config -> IO a
