{-# LANGUAGE ScopedTypeVariables #-}

module Process.Peer
    ( runPeer
    ) where


import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Exception
import qualified Network.Socket as S (Socket, sClose)

import Torrent
import Torrent.Message (Handshake(..))

import Process
import ProcessGroup
import Process.Channel
import Process.Status (UpDownStat)
import Process.FileAgent (FileAgentMessage)
import Process.PieceManager (PieceManagerMessage)

import Process.Peer.Sender
import Process.Peer.Handler
import Process.Peer.Receiver
import Process.Peer.SenderQueue


runPeer :: S.Socket -> InfoHash -> PeerId -> [Capabilities] -> PieceArray -> Integer
        -> RateTVar
        -> TVar [UpDownStat]
        -> TChan FileAgentMessage
        -> TChan PeerNetworkMessage
        -> TChan PieceManagerMessage
        -> IO ()
runPeer socket infoHash peerId caps pieceArray numPieces rateV statV fileAgentChan peerNetworkChan pieceMChan = do
    dropbox  <- newEmptyTMVarIO
    sendChan <- newTChanIO
    fromChan <- newTChanIO

    let handshake = Handshake peerId infoHash []
    let allForOne =
            [ runPeerSender socket handshake dropbox fromChan
            , runPeerHandler infoHash pieceArray numPieces rateV statV sendChan fromChan pieceMChan
            , runPeerReceiver socket fromChan
            , runPeerSenderQueue dropbox sendChan fileAgentChan
            ]

    threadId <- myThreadId
    group    <- initGroup
    result   <- bracket_
        (atomically $ writeTChan peerNetworkChan $ Connect infoHash threadId fromChan)
        (do
            atomically $ writeTChan peerNetworkChan $ Disconnect threadId
            S.sClose socket
        )
        (runGroup group allForOne)

    case result of
        Left (e :: SomeException) -> throwIO e
        _ -> ioError (userError "Unexpected termination")



