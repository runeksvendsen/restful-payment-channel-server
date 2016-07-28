module Test.RunData where

import Test.GenData
import qualified Data.Aeson as JSON (eitherDecodeStrict)
import qualified Data.ByteString as BS
import qualified Network.Wreq.Session as Wreq
import Data.String.Conversions (cs)
import Control.Monad (forM_)


main :: IO ()
main =
    let printErrOnFail eitherPaySess =
            case eitherPaySess of
                Left e -> fail $ "failed to decode JSON test data from stdin: " ++ e
                Right pd -> return pd
    in
        fmap JSON.eitherDecodeStrict BS.getLine >>= printErrOnFail >>= runData


runData :: PaySessionData -> IO ()
runData (PaySessionData openURL payURLList closeURL) =
    Wreq.withAPISession $ \conn -> do
        Wreq.post conn (cs openURL) BS.empty    -- 1. Open
        forM_ (map cs payURLList)               -- 2. Pay loop
                (flip (Wreq.put conn) BS.empty)
        Wreq.delete conn (cs closeURL)          -- 3. Close
        return ()