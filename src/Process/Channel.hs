module Process.Channel
    ( StatusMessage(..)
    , StatusState(..)
    , TrackerMessage(..)
    , PeerHandlerMessage(..)
    , PeerChokeMessage(..)
    , PeerNetworkMessage(..)
    , PeerRate(..)
    , RateTVar
    ) where

import Control.Concurrent
import Control.Concurrent.STM

import Torrent
import Torrent.Message as TM


data StatusMessage
    = StatusTrackerStat
        { _trackerInfoHash   :: InfoHash
        , _trackerComplete   :: Maybe Integer
        , _trackerIncomplete :: Maybe Integer
        }
    | StatusAddTorrent InfoHash Integer (TChan TrackerMessage)
    | StatusRemoveTorrent InfoHash
    | StatusExistsTorrent InfoHash (TMVar Bool)
    | StatusCompletedPiece InfoHash Integer
    | StatusTorrentCompleted InfoHash
    | StatusRequestStatus InfoHash (TMVar StatusState)
    | StatusRequestStatistic (TMVar [(InfoHash, StatusState)])


data StatusState = StatusState
    { _left              :: Integer
    , _uploaded          :: Integer
    , _downloaded        :: Integer
    , _torrentComplete   :: Maybe Integer
    , _torrentIncomplete :: Maybe Integer
    , _torrentState      :: TorrentState
    , _statusTrackerChan :: TChan TrackerMessage
    }


data TrackerMessage
    = TrackerStop
    | TrackerStart
    | TrackerComplete
    | TrackerTick Integer


data PeerChokeMessage
    = ChokePeer
    | UnchokePeer
    | PieceComplete PieceNum


data PeerNetworkMessage
    = Connect InfoHash ThreadId (TChan PeerHandlerMessage)
    | Disconnect ThreadId


data PeerHandlerMessage
    = PeerHandlerFromPeer (Either TM.Handshake TM.Message) Integer -- download bytes
    | PeerHandlerFromSender Integer -- upload bytes
    | PeerHandlerFromChokeManager PeerChokeMessage
    | PeerHandlerTick


data PeerRate = PeerRate
    { _peerRateUpRate     :: Double
    , _peerRateDownRate   :: Double
    , _peerRateInterested :: Bool
    , _peerRateChokingUs  :: Bool
    } deriving (Show)

type RateTVar = TVar [(ThreadId, PeerRate)]


