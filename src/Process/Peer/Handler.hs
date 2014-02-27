module Process.Peer.Handler
    ( start
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State

import Network.Socket (Socket)

import Process
import Process.Peer.Chan
import Protocol.Peer

import Server hiding (start)
import qualified Server


data PConf = PConf
    { cSendChan :: TChan Message
    }

data PState = PState
    { weChoke           :: Bool
    , peerChoke         :: Bool
    , weInterested      :: Bool
    , peerInterested    :: Bool
    , isEndgame         :: Bool
    -- , peerPieceArray    :: Set PieceNum
    -- , interestingPieces :: Set PieceNum
    -- , pieceTickCounter      :: Int -- ^ Cчетчик, сбрасывается при получении сообщения piece
    -- , keeAliveTickCounter   :: Int -- ^ Счетчик, сбрасывается при любом сообщении для пира
    -- , upRate            :: Rate
    -- , downRate          :: Rate
    }



start :: TChan PeerMessage -> TChan Message
      -> (Reason -> IO ())
      -> IO (TMVar Reason)
start fromChan sendChan userTermination = do
    let conf = PConf sendChan
        state = initState
    Server.start (conf, state) fromChan server userTermination


initState = PState
    { weChoke = True
    , peerChoke = True
    , weInterested = False
    , peerInterested = False
    , isEndgame = False
    }


-- TODO timeout => close connection
server ::  Server (PConf, PState) PeerMessage
server = dummyServer
    { srvOnMessage = onMessage
    }

onMessage state (PeerHandshake handshake) = do
    return $ Right state

onMessage (conf, state) (PeerMessage message) = do
    state' <- execProcess conf state (handleMessage message)
    return $ Right (conf, state')


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


