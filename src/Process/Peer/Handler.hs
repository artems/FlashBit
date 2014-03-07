module Process.Peer.Handler
    ( runHandler
    , specHandler
    ) where

import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader (asks)

import Server
import Process
import Supervisor
import Protocol.Peer

import Process.Peer.Chan


data PConf = PConf
    { cStopVar  :: TMVar ()
    , cSendChan :: TChan Message
    , cFromChan :: TChan PeerMessage
    }

data PState = PState
    { weChoke           :: Bool
    , peerChoke         :: Bool
    , weInterested      :: Bool
    , peerInterested    :: Bool
    , isEndgame         :: Bool
    -- , peerPieceArray    :: Set PieceNum
    -- , interestingPieces :: Set PieceNum
    -- , upRate            :: Rate
    -- , downRate          :: Rate
    }


mkState :: PState
mkState = PState
    { weChoke        = True
    , peerChoke      = True
    , weInterested   = False
    , peerInterested = False
    , isEndgame      = False
    }


runHandler :: TChan PeerMessage -> TChan Message -> TMVar () -> IO Reason
runHandler fromChan sendChan stop =
    runServer conf state startup server
  where
    conf = PConf stop sendChan fromChan
    state = mkState
    server = mkServer wait receive terminate
    startup = return ()
    terminate = \_ -> return ()


specHandler :: TChan PeerMessage -> TChan Message -> IO ChildSpec
specHandler fromChan sendChan = do
    stop <- newEmptyTMVarIO
    return $ ChildSpec
        { csType = Worker
        , csAction = runHandler fromChan sendChan stop
        , csRestart = Transient
        , csShutdown = atomically $ putTMVar stop ()
        , csShutdownTimeout = 100
        }


wait :: Process PConf PState (Either () PeerMessage)
wait = do
    stop <- asks cStopVar
    chan <- asks cFromChan
    liftIO . atomically $ orElse
        (takeTMVar stop >>= return . Left)
        (readTChan chan >>= return . Right)


receive :: Either () PeerMessage -> Process PConf PState ()
receive (Left _) = stopProcess Shutdown
receive (Right (PeerMessage message)) = do
    liftIO . print $ message
    -- handleMessage message
receive (Right (PeerHandshake handshake)) = do
    liftIO . print $ handshake
    return ()


handleMessage :: Message -> Process PConf PState ()
handleMessage message = do
    case message of
        KeepAlive
            -> return ()
        Choke
            -> modify (\s -> s { peerChoke = True  })
        Unchoke
            -> modify (\s -> s { peerChoke = False })
        Interested
            -> modify (\s -> s { peerInterested = True  })
        NotInterested
            -> modify (\s -> s { peerInterested = False })
        Have piecenum
            -> handleMessageHave piecenum
        BitField bitfield
            -> handleMessageBitField bitfield
        Request piecenum block
            -> handleMessageRequest piecenum block
        Piece piecenum offset bs
            -> handleMessagePiece piecenum offset bs
        Cancel piecenum block
            -> handleMessageCancel piecenum block
        Port _ -> return () -- No DHT yet, ignore


handleMessageHave piecenum = undefined
handleMessageBitField bitfield = undefined
handleMessageRequest piecenum block = undefined
handleMessagePiece piecenum offset bs = undefined
handleMessageCancel piecenum block = undefined


