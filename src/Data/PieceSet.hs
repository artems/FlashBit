module Data.PieceSet
    ( PieceSet
    , PieceSetFreezed
    , new
    , null
    , full
    , size
    , have
    , unhave
    , exist
    , exist'
    , freeze
    , toList
    , fromList
    , toSet
    ) where

import Prelude hiding (all, null)

import Data.Array.IO hiding (freeze)
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.IO as AIO
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)

import Torrent (PieceNum)


newtype PieceSet = PieceSet (IOUArray Integer Bool)

newtype PieceSetFreezed = PieceSetFreezed (UArray Integer Bool)


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

exist :: MonadIO m => PieceNum -> PieceSet -> m Bool
exist pieceNum (PieceSet ps) = liftIO $ readArray ps pieceNum

exist' :: PieceNum -> PieceSetFreezed -> Bool
exist' pieceNum (PieceSetFreezed ps) = ps ! pieceNum

freeze :: MonadIO m => PieceSet -> m PieceSetFreezed
freeze (PieceSet ps) = liftIO $ PieceSetFreezed `fmap` AIO.freeze ps

toList :: MonadIO m => PieceSet -> m [PieceNum]
toList (PieceSet ps) = liftIO $ do
    elems <- getAssocs ps
    return [pieceNum | (pieceNum, pieceExists) <- elems, pieceExists == True]

fromList :: MonadIO m => Integer -> [PieceNum] -> m PieceSet
fromList n pieces = liftIO $ do
    ps <- newArray (0, n - 1) False
    mapM_ (\pieceNum -> writeArray ps pieceNum True) pieces
    return $ PieceSet ps

toSet :: MonadIO m => PieceSet -> m (S.Set PieceNum)
toSet ps = S.fromDistinctAscList `liftM` toList ps
