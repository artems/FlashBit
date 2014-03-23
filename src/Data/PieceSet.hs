module Data.PieceSet
    ( PieceSet
    , new
    , null
    , full
    , size
    , have
    , unhave
    , exists
    , toList
    , fromList
    ) where


import Prelude hiding (all, null)

import Data.Array.IO
import qualified Data.List as L
import Control.Monad.Trans (MonadIO, liftIO)

import Torrent (PieceNum)


newtype PieceSet = PieceSet { unPS :: IOUArray Integer Bool }


new :: MonadIO m => Integer -> m PieceSet
new n = liftIO $ PieceSet `fmap` newArray (0, n - 1) False

all :: MonadIO m => (Bool -> Bool) -> PieceSet -> m Bool
all f (PieceSet ps) = liftIO $ L.all f `fmap` getElems ps

null :: MonadIO m => PieceSet -> m Bool
null = all (== False)

full :: MonadIO m => PieceSet -> m Bool
full = all (== True)

size :: MonadIO m => PieceSet -> m Integer
size (PieceSet ps) = liftIO $ L.foldl' f 0 `fmap` getElems ps
  where
    f acc pn = if pn then acc + 1 else acc

have :: MonadIO m => PieceNum -> PieceSet -> m ()
have pieceNum (PieceSet ps) = liftIO $ writeArray ps pieceNum True

unhave :: MonadIO m => PieceNum -> PieceSet -> m ()
unhave pieceNum (PieceSet ps) = liftIO $ writeArray ps pieceNum False

exists :: MonadIO m => PieceNum -> PieceSet -> m Bool
exists pieceNum (PieceSet ps) = liftIO $ readArray ps pieceNum

toList :: MonadIO m => PieceSet -> m [PieceNum]
toList (PieceSet ps) = liftIO $ do
    elems <- getAssocs ps
    return [pieceNum | (pieceNum, exists) <- elems, exists == True]

fromList :: MonadIO m => Integer -> [PieceNum] -> m PieceSet
fromList n pieces = liftIO $ do
    ps <- newArray (0, n - 1) False
    mapM_ (\pieceNum -> writeArray ps pieceNum False) pieces
    return $ PieceSet ps


