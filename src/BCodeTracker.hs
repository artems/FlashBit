module BCodeTracker
    ( trackerPeers
    , trackerError
    , trackerWarning
    , trackerInterval
    , trackerMinInterval
    , trackerComplete
    , trackerIncomplete
    ) where


import BCode

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)


trackerPeers :: BCode -> Maybe (ByteString, ByteString)
trackerPeers bc = do
    v4 <- searchStr "peers" bc
    v6 <- return $ fromMaybe (B.empty) (searchStr "peers6" bc)
    return (v4, v6)

trackerError :: BCode -> Maybe ByteString
trackerError = searchStr "failure reason"

trackerWarning :: BCode -> Maybe ByteString
trackerWarning = searchStr "warning mesage"

trackerInterval :: BCode -> Maybe Integer
trackerInterval = searchInt "interval"

trackerMinInterval :: BCode -> Maybe Integer
trackerMinInterval = searchInt "min interval"

trackerComplete :: BCode -> Maybe Integer
trackerComplete = searchInt "complete"

trackerIncomplete :: BCode -> Maybe Integer
trackerIncomplete = searchInt "incomplete"

