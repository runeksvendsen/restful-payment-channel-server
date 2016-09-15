module Common.Handler.Types where

import qualified Control.Monad.Reader as Reader
import qualified Servant

type AppM conf = Reader.ReaderT conf Servant.Handler