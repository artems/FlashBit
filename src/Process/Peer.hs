module Process.Peer
    (
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Concurrent.STM

import Process
import Protocol.Peer
import Protocol.Types
import qualified Server as Server


data PConf = PConf
    { cPeerChan      :: TChan PeerServerMessage
    -- , cSenderChan    :: TChan SenderMessage
    , cInfoHash      :: InfoHash
    , cPieceArray    :: PieceArray
    -- , peerManagerChan
    -- , pieceManagerChan
    -- , statTVar :: TVar a
    -- , rateTVar :: Tvar a
    -- , haveTVar       :: TVar a
    }

data PState = PState
    { weChoke           :: Bool
    , weInterested      :: Bool
    , peerChoke         :: Bool
    , peerInterested    :: Bool
    , isEndgame         :: Bool
    , peerPieceArray    :: Set PieceNum
    , interestingPieces :: Set PieceNum
    , pieceTickCounter      :: Int -- ^ Cчетчик, сбрасывается при получении сообщения piece
    , keeAliveTickCounter   :: Int -- ^ Счетчик, сбрасывается при любом сообщении для пира
    -- , upRate            :: Rate
    -- , downRate          :: Rate
    }


data PeerServerState
    = PeerServerState PConf PState

data PeerServerMessage
    = FromPeer Message
    | Tick

peerServer :: Server.Server PeerServerState PeerServerMessage
peerServer = Server.dummyServer
    { Server.srvOnMessage = onMessage
    , Server.srvTerminate = terminate
    }


terminate :: PeerServerState -> Server.Reason -> IO Server.Reason
terminate state reason = do
    -- let peerPieces = Array.assocs (sPeerHavePiece state)
    --     havePieces = foldl' (\acc (pn, have) -> if have then pn:acc else acc) [] peerPieces

    -- let pieceManagerChan = cPieceManagerChan conf
    -- atomically $ writeTChan pieceManagerChan (PeerUnhave havePieces)

    -- pid <- myThreadId
    -- let peerManagerChan = cPeerManagerChan conf
    -- atomically $ writeTChan peerManagerChan (Disconnect pid)
    return reason


onMessage :: PeerServerState -> PeerServerMessage -> IO (Server.Response PeerServerState)
onMessage (PeerServerState conf state) (FromPeer m) = do
    let proc = handlePeerMessage m
    state' <- execProcess conf state proc
    return (Right $ PeerServerState conf state')


handlePeerMessage = undefined

{-
handleTick :: Process PConf PState ()
handleTick = do
    checkKeepAlive
    -- updateStatus up down
    chan <- asks cPeerChan
    _ <- setTimeout 5000 (atomically $ writeTChan chan Tick)
    return ()


keepAlive :: Process PConf PState ()
keepAlive = do
    counter <- gets keepAliveCounter
    if counter > 24 -- 2 min
        then sendMessage (Sender.SenderMessage KeepAlive)
        else modify $ \s -> s { keepAliveCounter = succ counter }


sendMessage :: Sender.SenderMessage -> Process PConf PState ()
sendMessage message = do
    chan <- asks senderChan
    liftIO . atomically $ writeTChan chan message
    modify $ \s -> s { keepAliveCounter = 0 }

-}
{-
handlePeerMessage :: Message -> Process PConf PState ()
handlePeerMessage msg = do
    case msg of
        KeepAlive
            -> return ()
        Choke
            -> modify (\s -> s { peerChoke = True       })
        Unchoke
            -> modify (\s -> s { peerChoke = False      })
        Interested
            -> modify (\s -> s { peerInterested = True  })
        NotInterested
            -> modify (\s -> s { peerInterested = False })
        Have piecenum
            -> handlePeerHave piecenum
        BitField bitfield
            -> handlePeerBitField bitfield
        Request piecenum block
            -> handlePeerRequest piecenum block
        Piece piecenum offset bs
            -> handlePeerPiece piecenum offset bs
        Cancel piecenum block
            -> handlePeerCancel piecenum block
        Port _ -> return () -- No DHT yet, ignore


handlePeerHave :: PeiceNum -> Process PConf PState
handlePeerHave piecenum = do
    pieceMax <- asks pieceNumMax
    if piecenum <= pieceMax
        then do
            debugP $ "Пир сообщает, что у него появилась часть #" ++ show piecenum
            modify $ \s -> s {
                    peerPieces = Map.insert piecenum True (peerPieces s)
                    missingPieces = missingPieces s - 1
                }
            updateInterested [piecenum]
        else do
            errorP $ "Ошибочная часть #" ++ show piecenum ++", максимальная: " ++ pieceMax
            exit InternalError


handlePeerBitField :: ByteString -> Process PConf PState ()
handlePeerBitField bitfield = do
    pieces <- gets peerPieces
    if Map.null pieces
        then do
            numPieces <- (+1) `fmap` asks pieceNumMax
            peerPieces <- createPeerPieces numPieces bitfield
            modify $ \s -> s { sPeerPieces = peerPieces }
            updateInterested (Map.assoc peerPieces)
            modify $ \s -> s { missingPieces = missingPieces s - Map.size peerPieces }
        else do
            errorP "сообщение bitfield получено уже после начала обмена"
            exit InternalError


updateInterested :: [PieceNum] -> Process PConf PState ()
updateInterested piecenum = do
    have <- asks haveVar
    sendPieceManager (PeerHave piecenum have)
    newInterestingPieces <- liftIO . atomically $ takeTMVar have
    interestSet <- gets interestingPieces
    let interestSet' = update interestSet newInterestingPieces
    if S.null interestSet'
        then do
            debugP $ "Пир нам не интересен"
            modify $ \s -> s { interestingPieces = S.empty }
        else do
            debugP $ "Пир нам интересен"
            modify $ \s -> s { weInterested = True, interestingPieces = ns })
  where
    update set pieces = foldl' (flip S.insert) set pieces
-}



