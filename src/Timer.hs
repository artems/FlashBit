module Timer
    ( TimerId
    , maxTimeout
    , setTimeout
    , clearTimeout
    ) where

import Control.Concurrent


type TimerId = ThreadId

factor :: Int
factor = 1000000

maxTimeout :: Int
maxTimeout = (maxBound :: Int) `div` factor

clearTimeout :: TimerId -> IO ()
clearTimeout tid = killThread tid

setTimeout :: Int -> IO a -> IO TimerId
setTimeout delay action = forkIO proc
  where
    proc = do
        -- (0 <= delay <= maxDelay)
        let realDelay = (min (max delay 0) maxTimeout) * factor
        threadDelay realDelay
        action >> return ()
