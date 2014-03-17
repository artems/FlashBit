module BCodeTracker
    ( trackerPeers
    , trackerError
    , trackerWarning
    , trackerInterval
    , trackerMinInterval
    , trackerComplete
    , trackerIncomplete
    ) where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Maybe (fromMaybe)

import BCode
import BCodeAccess


trackerPeers :: BCode -> Maybe (ByteString, ByteString)
trackerPeers bc = do
    v4 <- get "peers" bc >>= getString
    v6 <- return $ fromMaybe (B.empty) (get "peers6" bc >>= getString)
    return (v4, v6)

trackerError :: BCode -> Maybe ByteString
trackerError = get "failure reason" >=> getString

trackerWarning :: BCode -> Maybe ByteString
trackerWarning = get "warning mesage" >=> getString

trackerInterval :: BCode -> Maybe Integer
trackerInterval = get "interval" >=> getNumber

trackerMinInterval :: BCode -> Maybe Integer
trackerMinInterval = get "min interval" >=> getNumber

trackerComplete :: BCode -> Maybe Integer
trackerComplete = get "complete" >=> getNumber

trackerIncomplete :: BCode -> Maybe Integer
trackerIncomplete = get "incomplete" >=> getNumber


