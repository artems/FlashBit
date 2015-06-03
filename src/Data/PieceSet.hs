module Data.PieceSet
    ( PieceSet
    , new
    , null
    , full
    , size
    , have
    , unhave
    , exist
    , toSet
    , toList
    , fromList
    ) where

import Prelude hiding (all, null)
import qualified Data.Set as S
import Torrent (PieceNum)


data PieceSet = PieceSet Integer (S.Set PieceNum)

new :: Integer -> PieceSet
new n = PieceSet n S.empty

null :: PieceSet -> Bool
null (PieceSet _ s) = S.null s

full :: PieceSet -> Bool
full (PieceSet n s) = n == fromIntegral (S.size s)

size :: PieceSet -> Integer
size (PieceSet _ s) = fromIntegral (S.size s)

have :: PieceNum -> PieceSet -> PieceSet
have pieceNum (PieceSet n s) = PieceSet n (S.insert pieceNum s)

unhave :: PieceNum -> PieceSet -> PieceSet
unhave pieceNum (PieceSet n s) = PieceSet n (S.delete pieceNum s)

exist :: PieceNum -> PieceSet -> Bool
exist pieceNum (PieceSet _ s) = S.member pieceNum s

toSet :: PieceSet -> (S.Set PieceNum)
toSet (PieceSet _ s) = s

toList :: PieceSet -> [PieceNum]
toList (PieceSet _ s) = S.toList s

fromList :: Integer -> [PieceNum] -> PieceSet
fromList n pieces = PieceSet n (S.fromList pieces)
