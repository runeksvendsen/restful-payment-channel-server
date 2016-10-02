module Util.Servant.Run where

import qualified Control.Monad.Trans.Except as ExceptT
import qualified Servant.Client as SC
import           Data.EitherR (fmapL)


runReq :: ExceptT.ExceptT SC.ServantError IO a -> IO (Either String a)
runReq = fmap (fmapL show) . ExceptT.runExceptT
