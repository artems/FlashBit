module FlashBit.TorrentThreadDatabase
    ( TorrentThreadDatabaseTVar
    , mkTorrentThreadDatabaseSTM
    , addTorrentThreadSTM
    , removeTorrentThreadSTM
    , getTorrentThreadSTM
    , getTorrentTrackerChanSTM
    , getThreadsSTM
    ) where

import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM

import Torrent
import ProcessGroup
import FlashBit.Tracker (TrackerMessage)


data TorrentThread = TorrentThread
    { _thread        :: (ProcessGroup, MVar ())
    , _trackerChan   :: TChan TrackerMessage
    }

type TorrentThreadDatabase = M.Map InfoHash TorrentThread

type TorrentThreadDatabaseTVar = TVar TorrentThreadDatabase

mkTorrentThread :: (ProcessGroup, MVar ())
                -> TChan TrackerMessage
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
                    -> TChan TrackerMessage
                    -> STM ()
addTorrentThreadSTM tv infoHash (group, stopM) trackerChan = do
    database <- readTVar tv
    let thread = mkTorrentThread (group, stopM) trackerChan
    writeTVar tv $ M.insert infoHash thread database

removeTorrentThreadSTM :: TorrentThreadDatabaseTVar -> InfoHash -> STM ()
removeTorrentThreadSTM tv infoHash = do
    database <- readTVar tv
    writeTVar tv $ M.delete infoHash database

getTorrent :: TorrentThreadDatabaseTVar -> InfoHash -> (TorrentThread -> a) -> STM (Maybe a)
getTorrent tv infoHash f = do
    database <- readTVar tv
    return $ f `fmap` M.lookup infoHash database

getTorrentThreadSTM :: TorrentThreadDatabaseTVar -> InfoHash -> STM (Maybe (ProcessGroup, MVar ()))
getTorrentThreadSTM tv infoHash = getTorrent tv infoHash _thread

getTorrentTrackerChanSTM :: TorrentThreadDatabaseTVar -> InfoHash -> STM (Maybe (TChan TrackerMessage))
getTorrentTrackerChanSTM tv infoHash = getTorrent tv infoHash _trackerChan

getThreadsSTM :: TorrentThreadDatabaseTVar -> STM [(InfoHash, (ProcessGroup, MVar ()))]
getThreadsSTM tv = (map (\(i, t) -> (i, _thread t)) . M.assocs) `fmap` readTVar tv
