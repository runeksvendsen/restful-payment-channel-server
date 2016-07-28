module Test.Cabal where

import Test.RunData
import Test.GenData

import System.Exit (exitSuccess, exitFailure)

main = do
    runData
    exitFailure
