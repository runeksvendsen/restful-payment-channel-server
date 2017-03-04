module AppPrelude.Util.HTTP where

import           Prelude hiding (userError)

import           AppPrelude.Types
import qualified RBPCP.Types as RBPCP

import           Data.String.Conversions (cs)
import qualified Data.Aeson as JSON

-- New
import qualified Control.Monad.Error.Class as Except
import           Servant
import qualified Data.Serialize as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Network.Haskoin.Transaction as HT
import qualified Network.Haskoin.Script as HS
import qualified Network.Haskoin.Crypto as HC

import qualified  PaymentChannel.Types as PayChan


--- HTTP error
userError' :: String -> AppM conf a
userError' = errorWithDescription 400

internalError :: String -> AppM conf a
internalError = errorWithDescription 400

onLeftThrow500 :: Either String a -> AppM conf a
onLeftThrow500   = either internalError return

errorWithDescription :: Int -> String -> AppM conf a
errorWithDescription code =
    Except.throwError . mkPaymentErr code . cs

mkPaymentErr :: Int -> String -> ServantErr
mkPaymentErr code e = mkServantError RBPCP.PaymentError code (cs e)

applicationError :: String -> AppM conf a
applicationError msg =
    Except.throwError $
        mkServantError RBPCP.ApplicationError 410 (cs msg)
        -- TODO: application error HTTP status code?

mkServantError :: RBPCP.ErrorType -> Int -> String -> ServantErr
mkServantError errType code msg =
    ServantErr
        {  errHTTPCode = code
        ,  errReasonPhrase = cs msg
        ,  errBody = cs responseBody
        ,  errHeaders = []
        }
    where responseBody = JSON.encode $ RBPCP.Error errType (cs msg)
