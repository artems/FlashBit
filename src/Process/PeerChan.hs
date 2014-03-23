module Process.PeerChan
    ( PeerHandlerMessage(..)
    , PeerChokeMessage(..)
    ) where


import Torrent
import qualified Torrent.Message as PM


data PeerHandlerMessage
    = FromPeer PM.Message
    | FromSender Int -- Always UpRate events
    | FromChokeManager PeerChokeMessage
    | PeerHandlerTick


data PeerChokeMessage
    = ChokePeer
    | UnchokePeer
    | PieceCompleted PieceNum
    | CancelBlock PieceNum PieceBlock

