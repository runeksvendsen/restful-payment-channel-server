module Test.RunData where

-- import Common.Util (pathParamEncode)
import Test.GenData
import Data.Aeson (eitherDecodeStrict, ToJSON(toJSON))
import qualified Data.ByteString as BS
import qualified Network.Wreq.Session as Wreq
import qualified Network.Haskoin.Constants as HCC
import Data.String.Conversions (cs)
import Control.Monad (forM_)


main :: IO ()
main =
    let printErrOnFail eitherPaySess =
            case eitherPaySess of
                Left e -> fail $ "failed to decode JSON test data from stdin: " ++ e
                Right pd -> return pd
    in do
        HCC.switchToTestnet3
        fmap eitherDecodeStrict BS.getLine >>= printErrOnFail >>= runData


runData :: PaySessionData -> IO ()
runData (PaySessionData uri closeURI payList) =
    Wreq.withAPISession $ \conn -> do
        Wreq.post conn (cs uri) (toJSON $ head payList) -- 1. Open
        forM_ (map toJSON (tail payList))               -- 2. Pay loop
                (Wreq.put conn (cs uri))
        Wreq.put conn (cs closeURI) (toJSON $ last payList)      -- 3. Close
        return ()
