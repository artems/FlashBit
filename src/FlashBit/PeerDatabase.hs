module FlashBit.PeerDatabase
    ( PeerTVar
    , PeerState(..)
    , PeerDatabase
    , PeerDatabaseTVar
    , mkPeerState
    , mkPeerDatabaseSTM
    , freeze
    , addPeerSTM
    , removePeerSTM
    , sizeSTM
    , isSeeder
    , isSeederSTM
    , isWeChokingSTM
    , isChokingUsSTM
    , isInterestedInUsSTM
    , getSpeedSTM
    , getNumPiecesSTM
    , getPeerPiecesSTM
    , isPieceSetEmptySTM
    , numToQueueSTM
    , queuePiecesSTM
    , setChokeSTM
    , setUnchokeSTM
    , setEndgameSTM
    , trackInterestedStateSTM
    , trackNotInterestedStateSTM
    , receiveChokeSTM
    , receiveUnchokeSTM
    , receiveInterestedSTM
    , receiveNotInterestedSTM
    , receiveHaveSTM
    , receiveBitfieldSTM
    , receivePieceSTM
    , incUploadCounterSTM
    , incDownloadCounterSTM
    , extractRateSTM
    ) where

import Control.Monad
import Control.Concurrent.STM

import Data.Rate
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as T
import qualified Data.PieceSet as PS
import qualified Data.ByteString as B
import qualified Data.Time.Clock as Time
import qualified Network.Socket as S

import Torrent
import Torrent.Message

import FlashBit.Peer.Common


data PeerState = PeerState
    { _weChokePeer          :: Bool
    , _peerChokeUs          :: Bool
    , _weInterestedInPeer   :: Bool
    , _peerInterestedInUs   :: Bool
    , _isEndgame            :: Bool
    , _blockQueue           :: S.Set (PieceNum, PieceBlock)
    , _peerPieces           :: PS.PieceSet
    , _interestedPieces     :: S.Set PieceNum
    , _upRate               :: Rate
    , _dnRate               :: Rate
    , _upSpeed              :: Double
    , _dnSpeed              :: Double
    , _numPieces            :: Integer
    , _missingPieces        :: Integer
    , _peerChan             :: TChan PeerHandlerMessage
    , _infoHash             :: InfoHash
    }

type PeerTVar = TVar PeerState

type PeerDatabase = M.Map S.SockAddr PeerState

type PeerDatabaseTVar = TVar (M.Map S.SockAddr PeerTVar)

mkPeerState :: InfoHash -> Integer -> TChan PeerHandlerMessage -> IO (TVar PeerState)
mkPeerState infoHash numPieces peerChan = do
    currentTime <- Time.getCurrentTime
    atomically . newTVar $ PeerState
        { _weChokePeer          = True
        , _peerChokeUs          = True
        , _weInterestedInPeer   = False
        , _peerInterestedInUs   = False
        , _isEndgame            = False
        , _blockQueue           = S.empty
        , _interestedPieces     = S.empty
        , _peerPieces           = PS.new numPieces
        , _upRate               = mkRate currentTime
        , _dnRate               = mkRate currentTime
        , _upSpeed              = 0
        , _dnSpeed              = 0
        , _numPieces            = numPieces
        , _missingPieces        = numPieces
        , _peerChan             = peerChan
        , _infoHash             = infoHash
        }

mkPeerDatabase :: M.Map S.SockAddr PeerTVar
mkPeerDatabase = M.empty

mkPeerDatabaseSTM :: STM PeerDatabaseTVar
mkPeerDatabaseSTM = newTVar mkPeerDatabase

get :: TVar a -> STM a
get = readTVar

gets :: (a -> b) -> TVar a -> STM b
gets f tv = f `fmap` get tv

modify :: (a -> a) -> TVar a -> STM ()
modify f tv = do
    st <- get tv
    writeTVar tv (f st)

modify' :: TVar a -> (a -> a) -> STM ()
modify' = flip modify

freeze :: PeerDatabaseTVar -> IO PeerDatabase
freeze peerDatabaseTV = do
    peerDatabase <- atomically $ readTVar peerDatabaseTV
    T.mapM getPeer peerDatabase
  where
    getPeer peerTV = atomically $ readTVar peerTV

addPeerSTM :: PeerDatabaseTVar -> PeerTVar -> S.SockAddr -> STM ()
addPeerSTM tv peerTV sockAddr = modify' tv $ M.insert sockAddr peerTV

removePeerSTM :: PeerDatabaseTVar -> S.SockAddr -> STM ()
removePeerSTM tv sockAddr = modify' tv $ M.delete sockAddr

sizeSTM :: PeerDatabaseTVar -> STM Integer
sizeSTM tv = (fromIntegral . M.size) `fmap` get tv

isSeeder :: PeerState -> Bool
isSeeder peer = _missingPieces peer == 0

isSeederSTM :: PeerTVar -> STM Bool
isSeederSTM tv = (== 0) `fmap` gets _missingPieces tv

isWeChokingSTM :: PeerTVar -> STM Bool
isWeChokingSTM = gets _weChokePeer

isChokingUsSTM :: PeerTVar -> STM Bool
isChokingUsSTM = gets _peerChokeUs

isInterestedInUsSTM :: PeerTVar -> STM Bool
isInterestedInUsSTM = gets _peerInterestedInUs

getSpeedSTM :: PeerTVar -> STM (Double, Double)
getSpeedSTM tv = do
    upSpeed <- gets _upSpeed tv
    dnSpeed <- gets _dnSpeed tv
    return (upSpeed, dnSpeed)

getNumPiecesSTM :: PeerTVar -> STM Integer
getNumPiecesSTM = gets _numPieces

getPeerPiecesSTM :: PeerTVar -> STM PS.PieceSet
getPeerPiecesSTM = gets _peerPieces

isPieceSetEmptySTM :: PeerTVar -> STM Bool
isPieceSetEmptySTM tv = PS.null `fmap` gets _peerPieces tv

setChokeSTM :: PeerTVar -> STM ()
setChokeSTM = modify $ \s -> s { _weChokePeer = True }

setUnchokeSTM :: PeerTVar -> STM ()
setUnchokeSTM = modify $ \s -> s { _weChokePeer = False }

setEndgameSTM :: PeerTVar -> STM ()
setEndgameSTM = modify $ \s -> s { _isEndgame = True }

receiveChokeSTM :: PeerTVar -> STM [(PieceNum, PieceBlock)]
receiveChokeSTM tv = do
    blockQueue <- gets _blockQueue tv
    modify' tv $ \s -> s
        { _blockQueue  = S.empty
        , _peerChokeUs = True
        }
    return $ S.toList blockQueue

receiveUnchokeSTM :: PeerTVar -> STM ()
receiveUnchokeSTM = modify $ \s -> s { _peerChokeUs = False }

receiveInterestedSTM :: PeerTVar -> STM ()
receiveInterestedSTM = modify $ \s -> s { _peerInterestedInUs = True }

receiveNotInterestedSTM :: PeerTVar -> STM ()
receiveNotInterestedSTM = modify $ \s -> s { _peerInterestedInUs = False }

receiveHaveSTM :: PieceNum -> PeerTVar -> STM ()
receiveHaveSTM pieceNum tv = do
    modify' tv $ \s -> s { _peerPieces = PS.have pieceNum (_peerPieces s) }
    decMissingCounterSTM 1 tv

receiveBitfieldSTM :: B.ByteString -> PeerTVar -> STM [PieceNum]
receiveBitfieldSTM bs tv = do
    numPieces  <- gets _numPieces tv
    let bitfield   = decodeBitField bs
    let peerPieces = PS.fromList numPieces bitfield
    modify' tv $ \s -> s { _peerPieces = peerPieces }
    decMissingCounterSTM (fromIntegral (length bitfield)) tv
    return bitfield

decMissingCounterSTM :: Integer -> PeerTVar -> STM ()
decMissingCounterSTM n =
    modify $ \s -> s { _missingPieces = (_missingPieces s) - n }

trackInterestedStateSTM :: [PieceNum] -> PeerTVar -> STM Bool
trackInterestedStateSTM pieceNum tv = do
    set          <- gets _interestedPieces tv
    weInterested <- gets _weInterestedInPeer tv
    let set'     = updateInterestedSet set
    modify' tv $ \s -> s { _interestedPieces = set' }
    let interestedNow = not weInterested && not (S.null set')
    when interestedNow $
        modify' tv $ \s -> s { _weInterestedInPeer = True }
    return interestedNow
  where
    updateInterestedSet set = foldr S.insert set pieceNum

trackNotInterestedStateSTM :: [PieceNum] -> PeerTVar -> STM Bool
trackNotInterestedStateSTM pieceNum tv = do
    set          <- gets _interestedPieces tv
    weInterested <- gets _weInterestedInPeer tv
    let set'     = updateInterestedSet set
    modify' tv $ \s -> s { _interestedPieces = set' }
    let notInterestedNow = weInterested && S.null set'
    when notInterestedNow $
        modify' tv $ \s -> s { _weInterestedInPeer = False }
    return notInterestedNow
  where
    updateInterestedSet set = foldr S.delete set pieceNum

receivePieceSTM :: PieceNum -> Integer -> B.ByteString -> PeerTVar -> STM Bool
receivePieceSTM pieceNum offset bs tv = do
    blockQueue <- gets _blockQueue tv
    let block  = PieceBlock offset (fromIntegral $ B.length bs)
    let record = (pieceNum, block)
    modify' tv $ \s -> s { _blockQueue = S.delete record blockQueue }
    -- В режиме 'endgame' этот же блок может быть скачен с другого пира
    return $ S.member record blockQueue

queuePiecesSTM :: [(PieceNum, PieceBlock)] -> PeerTVar -> STM [(PieceNum, PieceBlock)]
queuePiecesSTM toQueue tv = do
    blockQueue <- gets _blockQueue tv
    modify' tv $ \s -> s { _blockQueue = S.union blockQueue (S.fromList toQueue) }
    -- В режиме 'endgame', в списке может находится уже запрощенный pieceNum
    return $ filter (filterBlock blockQueue) toQueue
  where
    filterBlock blockQueue (pieceNum, block) = S.notMember (pieceNum, block) blockQueue

normalLoMark :: Integer
normalLoMark = 5

normalHiMark :: Integer
normalHiMark = 25

endgameLoMark :: Integer
endgameLoMark = 2

endgameHiMark :: Integer
endgameHiMark = 5

numToQueueSTM :: PeerTVar -> STM Integer
numToQueueSTM tv = do
    choked     <- gets _peerChokeUs tv
    interested <- gets _weInterestedInPeer tv
    if (not choked && interested)
        then checkWatermarkSTM tv
        else return 0

checkWatermarkSTM :: PeerTVar -> STM Integer
checkWatermarkSTM tv = do
    queue   <- gets _blockQueue tv
    endgame <- gets _isEndgame tv
    let size   = fromIntegral $ S.size queue
        loMark = if endgame then endgameLoMark else normalLoMark
        hiMark = if endgame then endgameHiMark else normalHiMark
    return $ if (size < loMark) then (hiMark - size) else 0

incUploadCounterSTM :: Integer -> PeerTVar -> STM ()
incUploadCounterSTM num =
    modify $ \s -> s { _upRate = updateBytes num (_upRate s) }

incDownloadCounterSTM :: Integer -> PeerTVar -> STM ()
incDownloadCounterSTM num =
    modify $ \s -> s { _dnRate = updateBytes num (_dnRate s) }

extractRateSTM :: Time.UTCTime -> PeerTVar -> STM ((Integer, Double), (Integer, Double))
extractRateSTM currentTime tv = do
    upRate <- gets _upRate tv
    dnRate <- gets _dnRate tv
    let (upload, upSpeed,  upRate')  = extractRate currentTime upRate
        (download, dnSpeed, dnRate') = extractRate currentTime dnRate
    modify' tv $ \s -> s 
        { _upRate  = upRate'
        , _dnRate  = dnRate'
        , _upSpeed = upSpeed
        , _dnSpeed = dnSpeed
        }
    return ((upload, upSpeed), (download, dnSpeed))