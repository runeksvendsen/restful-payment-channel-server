module GenDataExe where

import Test.GenData (genData)

import Options.Applicative.Common
import Options.Applicative.Extra
import Options.Applicative.Builder

import qualified Data.ByteString as BS
import qualified Network.Haskoin.Constants as HCC
import           Data.Aeson         (toJSON, encode)
import qualified Data.Text as T
import Data.String.Conversions (cs)
import Control.Monad (unless)
import Control.Monad.Reader (ask)
import Data.Int (Int64)
import Text.Read (reads)
import Data.Maybe (listToMaybe)


data Config = Config
  { endpoint        :: String
  , serverPubKey    :: String
  , num_payments    :: Int64
  , testnet         :: Bool }
--   , no_close        :: Bool}

sample :: Parser Config
sample = Config
    <$> strOption
        ( long "endpoint"
        <> metavar "ENDPOINT"
        <> help "Server endpoint, eg. \"https://pay.example.com\" or \"http://localhost:8000\"" )
    <*> strOption
        ( long "pubkey"
        <> metavar "PUBKEY"
        <> help "Server pubkey, hex-encoded 33 byte compressed pubkey" )
    <*> option (str >>= parseInt64)
      ( long "pay-count"
          <> metavar "PAYCOUNT"
          <> help "Number of payments to generate" )
    <*> switch
      ( long "testnet"
     <> help "Generate data for Bitcoin testnet3" )


runGen :: Config -> IO ()
runGen (Config endpoint pubKey numPayments _) = do
--     unless (not onTestnet)
    HCC.switchToTestnet3
    genData (cs endpoint) (fromIntegral numPayments) pubKey >>=
        BS.putStr . cs . encode . toJSON


main :: IO ()
main = execParser opts >>= runGen
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Generate JSON test data/URLs"
     <> header "RESTful Bitcoin payment channel protocol" )



parseInt64 :: String -> ReadM Int64
parseInt64 s = return $ read (s :: String) -- TODO: error handling
