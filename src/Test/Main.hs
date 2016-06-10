module Test.Main where

import Test.GenData (genData)

import Options.Applicative.Common
import Options.Applicative.Extra
import Options.Applicative.Builder

import qualified Network.Haskoin.Constants as HCC
import qualified Data.Text as T
import Data.String.Conversions (cs)
import Control.Monad (unless)

data Config = Config
  { endpoint :: T.Text
--   , num_payments :: Int
  , testnet :: Bool }

sample :: Parser Config
sample = Config . cs
    <$> strOption
        ( long "endpoint"
        <> metavar "ENDPOINT"
        <> help "Server endpoint, eg. \"https://pay.example.com\" or \"http://localhost:8000\"" )

  <*> switch
      ( long "testnet"
     <> help "Generate data for Bitcoin testnet3" )

runGen :: Config -> IO ()
runGen (Config endpoint onTestnet) = do
    unless (not onTestnet)
        HCC.switchToTestnet3
    genData endpoint


main :: IO ()
main = execParser opts >>= runGen
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Generate JSON test data/URLs"
     <> header "RESTful Bitcoin payment channel protocol" )