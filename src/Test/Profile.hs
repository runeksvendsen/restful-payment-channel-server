module Test.Profile where

import qualified Control.Exception as E
import           Control.Concurrent (ThreadId, throwTo, threadDelay, forkIO)
import           Control.Monad (void)

-- |Used to stop the server in case we're profiling,
--  so that any profiling data is written to disk
profile_selfDestruct :: ThreadId -> IO ()
profile_selfDestruct mainThreadId = void . forkIO $ do
    threadDelay 20000000
    putStrLn "Self-destruct. Killing main thread..."
    throwTo mainThreadId E.UserInterrupt
    return ()
