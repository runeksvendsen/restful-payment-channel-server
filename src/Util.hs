module Util where

-- | Accumulate items from a list until the given function returns False
--  Examples:
--  > accumulateWhile (\accumulatedItems _ -> sum accumulatedItems < 25) [1,2,3,4,5,6,7,8,9]
--  > [1,2,3,4,5,6]
--  > accumulateSentence = accumulateWhile (\_ c -> c /= '.')
accumulateWhile :: ([a] -> a -> Bool) ->  [a] -> [a]
accumulateWhile f l = accumulateWhileImpl [] l f

accumulateWhileImpl :: [a] -> [a] -> ([a] -> a -> Bool) -> [a]
accumulateWhileImpl accumList [] _                      = accumList
accumulateWhileImpl accumList (x:xs) accumulateThis =
    if accumulateThis accumList x then
        accumulateWhileImpl (accumList ++ [x]) xs accumulateThis
    else
        accumList

