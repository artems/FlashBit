{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FlashBit.Tracker.State
    ( TrackerState(..)
    , mkTrackerState
    , trackerStop
    , trackerStart
    , trackerComplete
    , trackerUpdateAnnounce
    , trackerEventTransition
    , trackerUpdateTimer
    , trackerCheckTick
    , getTrackerStatus
    ) where

import qualified Control.Monad.State as S
import qualified Data.ByteString as B

import Torrent
import Torrent.Announce


data TrackerState = TrackerState
    { _announceList  :: [[B.ByteString]]
    , _trackerStatus :: TrackerStatus
    , _nextTick      :: Integer
    }

type TrackerMonad a = (S.MonadState TrackerState m) => m a

mkTrackerState :: AnnounceList -> TrackerState
mkTrackerState announceList = TrackerState
    { _announceList  = announceList
    , _trackerStatus = Stopped
    , _nextTick      = 0
    }

trackerStatus :: TrackerStatus -> TrackerMonad ()
trackerStatus status = S.modify $ \s -> s { _trackerStatus = status }

trackerStop :: TrackerMonad ()
trackerStop = trackerStatus Stopped

trackerStart :: TrackerMonad ()
trackerStart = trackerStatus Started

trackerComplete :: TrackerMonad ()
trackerComplete = trackerStatus Completed

trackerUpdateAnnounce :: AnnounceList -> TrackerMonad ()
trackerUpdateAnnounce announceList =
    S.modify $ \s -> s { _announceList = announceList }

trackerEventTransition :: TrackerMonad ()
trackerEventTransition = do
    status <- S.gets _trackerStatus
    trackerStatus (newStatus status)
  where
    newStatus status = case status of
        Started   -> Running
        Stopped   -> Stopped
        Running   -> Running
        Completed -> Running

trackerUpdateTimer :: TrackerMonad Integer
trackerUpdateTimer = do
    nextTick <- (+1) `S.liftM` S.gets _nextTick
    S.modify $ \s -> s { _nextTick = nextTick }
    return nextTick

trackerCheckTick :: Integer -> TrackerMonad Bool
trackerCheckTick x = do
    tick   <- S.gets _nextTick
    status <- S.gets _trackerStatus
    return (status == Running && tick == x)

getTrackerStatus :: TrackerMonad TrackerStatus
getTrackerStatus = S.gets _trackerStatus
