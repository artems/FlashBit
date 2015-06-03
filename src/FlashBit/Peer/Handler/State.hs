{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module FlashBit.Peer.Handler.State
    ( PeerState(..)
    , mkPeerState
    , isPieceSetEmpty
    , isWeChoking
    , getPeerPieces
    , getNumPieces
    , numToQueue
    , queuePieces
    , setChoke
    , setUnchoke
    , setEndgame
    , trackInterestedState
    , trackNotInterestedState
    , receiveChoke
    , receiveUnchoke
    , receiveInterested
    , receiveNotInterested
    , receiveBitfield
    , receiveHave
    , receivePiece
    , incUploadCounter
    , incDownloadCounter
    , getRate
    ) where

import qualified Control.Monad.State as S
import qualified Data.Set as S
import qualified Data.PieceSet as PS
import qualified Data.ByteString as B
import qualified Data.Time.Clock as Time

import Data.Rate
import Torrent
import Torrent.Message


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
    , _numPieces            :: Integer
    , _missingPieces        :: Integer
    }

type PeerMonad a = (S.MonadIO m, S.MonadState PeerState m) => m a


mkPeerState :: Integer -> IO PeerState
mkPeerState numPieces = do
    currentTime   <- Time.getCurrentTime
    return $ PeerState
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
        , _numPieces            = numPieces
        , _missingPieces        = numPieces
        }


isWeChoking :: PeerMonad Bool
isWeChoking = S.gets _weChokePeer

getPeerPieces :: PeerMonad PS.PieceSet
getPeerPieces = S.gets _peerPieces

getNumPieces :: PeerMonad Integer
getNumPieces = S.gets _numPieces

isPieceSetEmpty :: PeerMonad Bool
isPieceSetEmpty = do
    ps <- S.gets _peerPieces
    return $ PS.null ps

setChoke :: PeerMonad ()
setChoke = S.modify $ \s -> s { _weChokePeer = True }

setUnchoke :: PeerMonad ()
setUnchoke = S.modify $ \s -> s { _weChokePeer = False }

setEndgame :: PeerMonad ()
setEndgame = S.modify $ \s -> s { _isEndgame = True }

receiveChoke :: PeerMonad [(PieceNum, PieceBlock)]
receiveChoke = do
    blockQueue <- S.gets _blockQueue
    S.modify $ \s -> s
        { _blockQueue  = S.empty
        , _peerChokeUs = True
        }
    return $ S.toList blockQueue

receiveUnchoke :: PeerMonad ()
receiveUnchoke = S.modify $ \s -> s { _peerChokeUs = False }

receiveInterested :: PeerMonad ()
receiveInterested = S.modify $ \s -> s { _peerInterestedInUs = True }

receiveNotInterested :: PeerMonad ()
receiveNotInterested = S.modify $ \s -> s { _peerInterestedInUs = False }

receiveHave :: PieceNum -> PeerMonad ()
receiveHave pieceNum = do
    S.modify $ \s -> s { _peerPieces = PS.have pieceNum (_peerPieces s) }
    decMissingCounter 1

receiveBitfield :: B.ByteString -> PeerMonad [PieceNum]
receiveBitfield bs = do
    numPieces  <- S.gets _numPieces
    let bitfield = decodeBitField bs
    let peerPieces = PS.fromList numPieces bitfield
    S.modify $ \s -> s { _peerPieces = peerPieces }
    decMissingCounter $ fromIntegral (length bitfield)
    return bitfield

decMissingCounter :: Integer -> PeerMonad ()
decMissingCounter n =
    S.modify $ \s -> s { _missingPieces = (_missingPieces s) - n }

trackInterestedState :: [PieceNum] -> PeerMonad Bool
trackInterestedState pieceNum = do
    set          <- S.gets _interestedPieces
    weInterested <- S.gets _weInterestedInPeer
    let set'     = updateInterestedSet set
    S.modify $ \s -> s { _interestedPieces = set' }
    let interestedNow = not weInterested && not (S.null set')
    S.when interestedNow $ S.modify $ \s -> s { _weInterestedInPeer = True }
    return interestedNow
  where
    updateInterestedSet set = foldr S.insert set pieceNum

trackNotInterestedState :: [PieceNum] -> PeerMonad Bool
trackNotInterestedState pieceNum = do
    set          <- S.gets _interestedPieces
    weInterested <- S.gets _weInterestedInPeer
    let set'     = updateInterestedSet set
    S.modify $ \s -> s { _interestedPieces = set' }
    let notInterestedNow = weInterested && S.null set'
    S.when notInterestedNow $ S.modify $ \s -> s { _weInterestedInPeer = False }
    return notInterestedNow
  where
    updateInterestedSet set = foldr S.delete set pieceNum

receivePiece :: PieceNum -> Integer -> B.ByteString -> PeerMonad Bool
receivePiece pieceNum offset bs = do
    blockQueue <- S.gets _blockQueue
    let block  = PieceBlock offset (fromIntegral $ B.length bs)
        record = (pieceNum, block)
    S.modify $ \s -> s { _blockQueue = S.delete record blockQueue }
    -- В режиме 'endgame' этот же блок может быть скачен с другого пира
    return $ S.member record blockQueue

queuePieces :: [(PieceNum, PieceBlock)] -> PeerMonad [(PieceNum, PieceBlock)]
queuePieces toQueue = do
    blockQueue <- S.gets _blockQueue
    S.modify $ \s -> s { _blockQueue = S.union blockQueue (S.fromList toQueue) }
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

numToQueue :: PeerMonad Integer
numToQueue = do
    choked     <- S.gets _peerChokeUs
    interested <- S.gets _weInterestedInPeer
    if (not choked && interested)
        then checkWatermark
        else return 0

checkWatermark :: PeerMonad Integer
checkWatermark = do
    queue   <- S.gets _blockQueue
    endgame <- S.gets _isEndgame
    let size   = fromIntegral $ S.size queue
        loMark = if endgame then endgameLoMark else normalLoMark
        hiMark = if endgame then endgameHiMark else normalHiMark
    return $ if (size < loMark) then (hiMark - size) else 0

incUploadCounter :: Integer -> PeerMonad ()
incUploadCounter num =
    S.modify $ \s -> s { _upRate = updateBytes num (_upRate s) }

incDownloadCounter :: Integer -> PeerMonad ()
incDownloadCounter num =
    S.modify $ \s -> s { _dnRate = updateBytes num (_dnRate s) }

getRate :: Time.UTCTime -> PeerMonad ((Integer, Double), (Integer, Double))
getRate currentTime = do
    upRate <- S.gets _upRate
    dnRate <- S.gets _dnRate
    let (upload, upspeed,  upRate')  = extractRate currentTime upRate
        (download, dnspeed, dnRate') = extractRate currentTime dnRate
    S.modify $ \s -> s { _upRate = upRate', _dnRate = dnRate' }
    return ((upload, upspeed), (download, dnspeed))
