module ConnManager.Servant where

import qualified Control.Monad.Trans.Except as ExceptT
import qualified Servant.Client as SC
import qualified Servant.Common.Req as SR
import           Data.EitherR (fmapL)

import           ConnManager.Types -- ConnManager2

runReq :: ExceptT.ExceptT SC.ServantError IO a -> IO (Either String a)
runReq = fmap (fmapL show) . ExceptT.runExceptT

runReq' :: ConnManager2 -> SC.ClientM a -> IO (Either String a)
runReq' (Conn2 baseUrl man) c =
    fmapL show <$> SR.runClientM c (SR.ClientEnv man baseUrl)

