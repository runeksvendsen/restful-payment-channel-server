{-# LANGUAGE  OverloadedStrings #-}

module CleanImports where

import           Prelude hiding (take)

import           System.Process
import           Data.Attoparsec.ByteString.Char8
import           Data.String.Conversions (cs)

type File = String

projectDir = "/Users/rune/IdeaProjects/restful-payment-channel-server/"
stackCmd = "stack exec -- ghc-mod check "
testFile = "src/Server/Handlers.hs" -- "Lib.hs"

main :: IO ()
main = do
    let ghcModCheckCmd = shell $ stackCmd ++ testFile
    res <- readCreateProcess (ghcModCheckCmd { cwd = Just projectDir }) ""

    putStrLn $ "Got dis: " ++ res
    return ()

data LineType = Warning | Unknown
data CheckLine = CheckLine File Word Word LineType String

parseLine = do
    file <- takeTill (== ':')
    take 1
    line <- decimal
    take 1
    col <- decimal
    take 1
    _ <- string "Warning"
    take 2
    msg <- takeTill (== '\n')
    take 1
    return $ CheckLine (cs file) line col Warning (cs msg)

-- src/Server/Handlers.hs:9:1:Warning: The import of ‘Server.Config.Types’ is redundant  except perhaps to import instances from ‘Server.Config.Types’To import instances alone, use: impor  PayChanServer.Config.Types()
-- src/Server/Handlers.hs:16:1:Warning: The import of ‘BlockchainAPI.Impl.ChainSo’ is redundant  except perhaps to import instances from ‘BlockchainAPI.Impl.ChainSo’To import instances alone, use: import BlockchainAPI.Impl.ChainSo()
-- src/Server/Handlers.hs:28:1:Warning: The import of ‘Snap’ is redundant  except perhaps to import instances from ‘Snap’To import instances alone, use: import Snap()
-- src/Server/Handlers.hs:34:1:Warning: The import of ‘setSenderChangeAddress’from module ‘Data.Bitcoin.PaymentChannel.Util’ is redundant
-- src/Server/Handlers.hs:36:1:Warning: The qualified import of ‘Network.Haskoin.Constants’ is redundant  except perhaps to import instances from ‘Network.Haskoin.Constants’To import instances alone, use: import Network.Haskoin.Constants()
-- src/Server/Handlers.hs:37:1:Warning: The import of ‘Control.Lens’ is redundant  except perhaps to import instances from ‘Control.Lens’To import instances alone, use: import Control.Lens()
-- src/Server/Handlers.hs:44:1:Warning: The qualified import of ‘Data.ByteString’ is redundant  except perhaps to import instances from ‘Data.ByteString’To import instances alone, use: import Data.ByteString()
-- src/Server/Handlers.hs:46:1:Warning: The import of ‘Text.Printf’ is redundant  except perhaps to import instances from ‘Text.Printf’To import instances alone, use: import Text.Printf()
-- src/Server/Handlers.hs:48:1:Warning: The import of ‘Test.GenData’ is redundant  except perhaps to import instances from ‘Test.GenData’To import instances alone, use: import Test.GenData()