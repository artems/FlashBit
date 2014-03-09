module BCodeTorrent
    ( comment
    , announce
    , creationDate
    , announceList
    , infoHash
    , infoName
    , infoFiles
    , infoLength
    , infoPieces
    , infoPieceCount
    , infoPieceLength
    ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import BCode
import Digest (digest)


comment :: BCode -> Maybe ByteString
comment = searchStr "comment"

announce :: BCode -> Maybe ByteString
announce = searchStr "announce"

creationDate :: BCode -> Maybe ByteString
creationDate = searchStr "creation date"

announceList :: BCode -> Maybe [[ByteString]]
announceList bc = do
    (BList announce') <- search [BCodePStr "announce-list"] bc
    tracker <- mapM extract1 announce'
    mapM (mapM extract2) tracker
  where
    extract1 (BList tracker) = Just tracker
    extract1 _               = Nothing
    extract2 (BStr url) = Just url
    extract2 _          = Nothing

infoName :: BCode -> Maybe ByteString
infoName bc = do
    (BStr name) <- search [BCodePStr "info", BCodePStr "name"] bc
    return name

infoHash :: BCode -> Maybe ByteString
infoHash bc = do
    info <- search [BCodePStr "info"] bc
    return . digest . encode $ info


infoFiles :: BCode -> Maybe [([String], Integer)]
infoFiles bc = files1 <|> files2
  where
    -- single-file torrent
    files1 = do
        name <- B8.unpack `fmap` infoName bc
        (BInt len) <- search [BCodePStr "info", BCodePStr "length"] bc
        return [([name], len)]

    -- multi-file torrent
    files2 = do
        (BList files) <- search [BCodePStr "info", BCodePStr "files"] bc
        forM files $ \bc_file -> do
            (BInt len) <- search [BCodePStr "length"] bc_file
            (BList pathCoded) <- search [BCodePStr "path"] bc_file
            let path = map (\(BStr s) -> B8.unpack s) pathCoded
            return (path, len)


infoLength :: BCode -> Maybe Integer
infoLength bc = length1 <|> length2
  where
    -- single-file torrent
    length1 = do
        (BInt len) <- search [BCodePStr "info", BCodePStr "length"] bc
        return len

    -- multi-file torrent
    length2 = do
        (BList files) <- search [BCodePStr "info", BCodePStr "files"] bc
        lengthBCode <- mapM (search [BCodePStr "length"]) files
        lengthInt <- mapM unBInt lengthBCode
        return (sum lengthInt)
      where
        unBInt (BInt i) = Just i
        unBInt _        = Nothing


infoPieces :: BCode -> Maybe [ByteString]
infoPieces bc = do
    (BStr pieces) <- search [BCodePStr "info", BCodePStr "pieces"] bc
    return . split $ pieces
  where
    split str
        | str == B.empty = []
        | otherwise = let
            (block, rest) = B.splitAt 20 str
            in block : split rest


infoPieceCount :: BCode -> Maybe Integer
infoPieceCount bc = do
    (BStr pieces) <- search [BCodePStr "info", BCodePStr "pieces"] bc
    return $ fromIntegral (B.length pieces) `div` 20


infoPieceLength :: BCode -> Maybe Integer
infoPieceLength bc = do
    (BInt length') <- search [BCodePStr "info", BCodePStr "piece length"] bc
    return length'


