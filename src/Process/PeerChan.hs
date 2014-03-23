module Process.PeerChan
    ( PeerHandlerMessage(..)
    , PeerChokeMessage(..)
    ) where


import Piece
import qualified PeerProtocol as PP


data PeerHandlerMessage
    = FromPeer PP.Message
    | FromSender Int -- Always UpRate events
    | FromChokeManager PeerChokeMessage
    | PeerHandlerTick


data PeerChokeMessage
    = ChokePeer
    | UnchokePeer
    | PieceCompleted PieceNum
    | CancelBlock PieceNum PieceBlock

