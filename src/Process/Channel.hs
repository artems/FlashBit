module Process.Channel
    ( StatusMessage(..)
    , StatusState(..)
    , TrackerMessage(..)
    ) where

import Control.Concurrent.STM

import Torrent


data StatusMessage
    = TrackerStat
        { trackerInfoHash   :: InfoHash
        , trackerComplete   :: Maybe Integer
        , trackerIncomplete :: Maybe Integer
        }
    | StatusAddTorrent InfoHash Integer (TChan TrackerMessage)
    | StatusRemoveTorrent InfoHash
    | ExistsTorrent InfoHash (TMVar Bool)
    | TorrentCompleted InfoHash
    | CompletedPiece InfoHash Integer
    | RequestStatus InfoHash (TMVar StatusState)
    | RequestStatistic (TMVar [(InfoHash, StatusState)])


data StatusState = StatusState
    { _sUploaded        :: Integer
    , _sDownloaded      :: Integer
    , _sLeft            :: Integer
    , _sComplete        :: Maybe Integer
    , _sIncomplete      :: Maybe Integer
    , _sState           :: TorrentState
    , _sTrackerChan     :: TChan TrackerMessage
    }


data TrackerMessage
    = TrackerStop           -- ^ Сообщить трекеру об остановки скачивания
    | TrackerStart          -- ^ Сообщить трекеру о начале скачивания
    | TrackerComplete       -- ^ Сообщить трекеру об окончании скачивания
    | TrackerTick Integer   -- ^ ?


