module Process.Channel
    ( StatusMessage(..)
    , StatusState(..)
    , TrackerMessage(..)
    , PeerHandlerMessage(..)
    , PeerChokeMessage(..)
    , PeerEventMessage(..)
    , PeerRate(..)
    , RateTVar
    ) where

import Control.Concurrent
import Control.Concurrent.STM

import Torrent
import Torrent.Message as TM


data StatusMessage
    = TrackerStat
        { _trackerInfoHash   :: InfoHash
        , _trackerComplete   :: Maybe Integer
        , _trackerIncomplete :: Maybe Integer
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



data PeerHandlerMessage
    = FromPeer TM.Message
    | FromSender Int -- Always UpRate events
    | FromChokeManager PeerChokeMessage
    | PeerHandlerTick


data PeerChokeMessage
    = ChokePeer
    | UnchokePeer
    | PieceCompleted PieceNum
    | CancelBlock PieceNum PieceBlock

data PeerEventMessage
    = Connect InfoHash ThreadId (TChan PeerHandlerMessage)
    | Disconnect ThreadId


data PeerRate = PeerRate
    { _peerUpRate     :: Double
    , _peerDownRate   :: Double
    , _peerInterested :: Bool
    , _peerSeeding    :: Bool
    , _peerSnubs      :: Bool
    , _peerChokingUs  :: Bool
    } deriving (Show)

type RateTVar = TVar [(ThreadId, PeerRate)]


