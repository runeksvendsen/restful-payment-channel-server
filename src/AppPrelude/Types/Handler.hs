module AppPrelude.Types.Handler where

import qualified Control.Monad.Reader as Reader
import qualified Servant


-- |We use this monad for the handlers, which gives access to configuration data
--  of type 'conf'.
type AppM conf = Reader.ReaderT conf Servant.Handler
