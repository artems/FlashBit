module FlashBit.TorrentThreadDatabase
    ( TorrentThreadDatabase
    , TorrentThreadDatabaseTVar
    , mkTorrentThread
    , mkTorrentThreadDatabase
    , mkTorrentThreadDatabaseSTM
    , addTorrentThreadSTM
    , removeTorrentThreadSTM
    , getTorrentThreadSTM
    , getTorrentTrackerChanSTM
    , getAllTorrentThreadsSTM
    ) where

import qualified Data.Map as M
import Control.Monad (liftM)
import Control.Concurrent
import Control.Concurrent.STM

import ProcessGroup
import Torrent
import qualified FlashBit.Tracker as Tracker


data TorrentThread = TorrentThread
    { _thread        :: (ProcessGroup, MVar ())
    , _trackerChan   :: TChan Tracker.TrackerMessage
    }

type TorrentThreadDatabase = M.Map InfoHash TorrentThread

type TorrentThreadDatabaseTVar = TVar TorrentThreadDatabase

mkTorrentThread :: (ProcessGroup, MVar ()) 
                -> TChan Tracker.TrackerMessage
                -> TorrentThread
mkTorrentThread (group, stopM) trackerChan = TorrentThread
    { _thread        = (group, stopM)
    , _trackerChan   = trackerChan
    }

mkTorrentThreadDatabase :: TorrentThreadDatabase
mkTorrentThreadDatabase = M.empty

mkTorrentThreadDatabaseSTM :: STM TorrentThreadDatabaseTVar
mkTorrentThreadDatabaseSTM = newTVar mkTorrentThreadDatabase

addTorrentThreadSTM :: TorrentThreadDatabaseTVar
                    -> InfoHash
                    -> (ProcessGroup, MVar ())
                    -> TChan Tracker.TrackerMessage
                    -> STM ()
addTorrentThreadSTM tv infoHash (group, stopM) trackerChan = do
    let thread = mkTorrentThread (group, stopM) trackerChan
    database <- readTVar tv
    writeTVar tv $ M.insert infoHash thread database

removeTorrentThreadSTM :: TorrentThreadDatabaseTVar -> InfoHash -> STM ()
removeTorrentThreadSTM tv infoHash = do
    database <- readTVar tv
    writeTVar tv $ M.delete infoHash database

getTorrent :: TorrentThreadDatabaseTVar -> InfoHash -> (TorrentThread -> a) -> STM (Maybe a)
getTorrent tv infoHash f = do
    db <- readTVar tv
    let torrent = M.lookup infoHash db
    return $ f `fmap` torrent

getTorrentThreadSTM :: TorrentThreadDatabaseTVar -> InfoHash -> STM (Maybe (ProcessGroup, MVar ()))
getTorrentThreadSTM tv infoHash = getTorrent tv infoHash _thread

getTorrentTrackerChanSTM :: TorrentThreadDatabaseTVar -> InfoHash -> STM (Maybe (TChan Tracker.TrackerMessage))
getTorrentTrackerChanSTM tv infoHash = getTorrent tv infoHash _trackerChan

getAllTorrentThreadsSTM :: TorrentThreadDatabaseTVar -> STM [(InfoHash, (ProcessGroup, MVar ()))]
getAllTorrentThreadsSTM tv = (map (\(i, t) -> (i, _thread t)) . M.assocs) `liftM` readTVar tv
