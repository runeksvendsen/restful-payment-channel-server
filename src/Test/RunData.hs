module Test.RunData where

import Test.GenData hiding (main)
import qualified Data.Aeson as JSON (eitherDecodeStrict)
import qualified Data.ByteString as BS
import qualified Network.Wreq as Wreq (asJSON, responseBody) --get, post, put, delete
import qualified Network.Wreq.Session as Wreq (post, put, withAPISession)
import Data.String.Conversions (cs)
import Control.Monad (forM_)



main :: IO ()
main = Wreq.withAPISession $
    \conn -> do
        eitherPaySess <- fmap JSON.eitherDecodeStrict BS.getLine
        (PaySessionData openURL payURLList) <-
            case eitherPaySess of
                Left e -> fail $ "failed to decode JSON test data from stdin: " ++ e
                Right pd -> return pd

        Wreq.post conn (cs openURL) BS.empty
        forM_ (map cs payURLList) (flip (Wreq.put conn) BS.empty)
