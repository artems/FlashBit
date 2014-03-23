module Data.PieceHistogram
    ( PieceHistogram
    , size
    , empty
    , have
    , unhave
    , remove
    , allHave
    , allUnhave
    , pick
    ) where


import Data.List (foldl')
import Data.PSQueue (PSQ, Binding((:->)))
import qualified Data.PSQueue as PSQ

import Torrent (PieceNum)


newtype PieceHistogram = PieceHistogram { unPS :: PSQ PieceNum Int }


size :: PieceHistogram -> Int
size = PSQ.size . unPS


empty :: PieceHistogram
empty = PieceHistogram PSQ.empty


have :: PieceNum -> PieceHistogram -> PieceHistogram
have pieceNum = PieceHistogram . PSQ.alter f pieceNum . unPS
  where
    f Nothing  = Just 1
    f (Just x) = Just (x + 1)


unhave :: PieceNum -> PieceHistogram -> PieceHistogram
unhave pieceNum = PieceHistogram . PSQ.alter f pieceNum . unPS
  where
    f Nothing  = Nothing
    f (Just 1) = Nothing
    f (Just x) = Just (x - 1)


remove :: PieceNum -> PieceHistogram -> PieceHistogram
remove pieceNum = PieceHistogram . PSQ.delete pieceNum . unPS


allHave :: [PieceNum] -> PieceHistogram -> PieceHistogram
allHave pieceNum ps = foldl' (\ps pn -> have pn ps) ps pieceNum


allUnhave :: [PieceNum] -> PieceHistogram -> PieceHistogram
allUnhave pieceNum ps = foldl' (\ps pn -> unhave pn ps) ps pieceNum


pick :: (PieceNum -> Bool) -> Int -> PieceHistogram -> [PieceNum]
pick acceptor numPick ps = pick' [] numPick $ PSQ.minView (unPS ps)
  where
    pick' acc 0 _       = acc
    pick' acc _ Nothing = acc
    pick' acc n (Just (pieceNum :-> _value, rest)) =
        case acceptor pieceNum of
            True  -> pick' (pieceNum : acc) (n - 1) $ PSQ.minView rest
            False -> pick' acc n $ PSQ.minView rest

