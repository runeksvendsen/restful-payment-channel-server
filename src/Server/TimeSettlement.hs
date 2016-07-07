module Server.TimeSettlement where

-- Time-based settlement
-- Close channels well before the channel expiration date

import Server.ChanStore

import           Control.Monad.Catch (bracket, finally, try)
import           Control.Concurrent  (forkIO, killThread, threadDelay)
import           Control.Monad (unless)

startSettlementThread ::
    ChannelMap
    -> Int -- ^ Check interval in seconds
    -- | Settlement period (hours).
    --  Channels will be closed this many hours before the specified expiration date.
    -> Word
    -> IO ()
startSettlementThread m i settlePeriod =
    putStrLn "Started disk sync thread." >>
    settlementThread m (i * round 1e6) settlePeriod

settlementThread ::
    ChannelMap
    -> Int -- ^ Check interval in microseconds
    -> Word
    -> IO ()
settlementThread m delay settlePeriod =
    (threadDelay delay >> settleExpiringChannels m settlePeriod)
        `finally` settlementThread m delay settlePeriod

settleExpiringChannels ::
    ChannelMap
    -> Word
    -> IO ()
settleExpiringChannels settlePeriod m = do
    eitherCount <- try $ undefined m
    case eitherCount of
        Left  e    -> putStrLn $ "ERROR: Disk sync failed! " ++ show (e :: IOException)
        Right syncCount ->
            unless (syncCount == 0) $
                    putStrLn $ "Synced " ++ show syncCount ++ " channel state(s) to disk."
