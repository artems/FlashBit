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

import Control.Monad ((>=>))
import Control.Applicative ((<|>))
import Control.Monad (forM)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import BCode
import BCodeAccess
import Digest (digest)


comment :: BCode -> Maybe ByteString
comment = get "comment" >=> getString

announce :: BCode -> Maybe ByteString
announce = get "announce" >=> getString

creationDate :: BCode -> Maybe ByteString
creationDate = get "creation date" >=> getString

announceList :: BCode -> Maybe [[ByteString]]
announceList bc = do
    announce' <- get "announce-list" bc >>= getList
    tracker <- mapM extract1 announce'
    mapM (mapM extract2) tracker
  where
    extract1 (BList tracker) = Just tracker
    extract1 _               = Nothing
    extract2 (BStr url) = Just url
    extract2 _          = Nothing

infoName :: BCode -> Maybe ByteString
infoName = get "info" >=> get "name" >=> getString

infoHash :: BCode -> Maybe ByteString
infoHash = get "info" >=> return . digest . encode


infoFiles :: BCode -> Maybe [([String], Integer)]
infoFiles bc = files1 <|> files2
  where
    -- single-file torrent
    files1 = do
        name <- B8.unpack `fmap` infoName bc
        len  <- return bc >>= get "info" >>= get "length" >>= getNumber
        return [([name], len)]

    -- multi-file torrent
    files2 = do
        files <- return bc >>= get "info" >>= get "files" >>= getList
        forM files $ \bc_file -> do
            len <- return bc_file >>= get "length" >>= getNumber
            pathCoded <- return bc_file >>= get "path" >>= getList
            let path = map (\(BStr s) -> B8.unpack s) pathCoded
            return (path, len)


infoLength :: BCode -> Maybe Integer
infoLength bc = length1 <|> length2
  where
    -- single-file torrent
    length1 = return bc >>= get "info" >>= get "length" >>= getNumber

    -- multi-file torrent
    length2 = do
        files   <- return bc >>= get "info" >>= get "files" >>= getList
        length' <- mapM (get "length" >=> getNumber) files
        return (sum length')


infoPieces :: BCode -> Maybe [ByteString]
infoPieces = get "info" >=> get "pieces" >=> getString >=> return . split
  where
    split str
        | B.null str = []
        | otherwise  =
            let (block, rest) = B.splitAt 20 str
            in  block : split rest


infoPieceCount :: BCode -> Maybe Integer
infoPieceCount bc = do
    pieces <- return bc >>= get "info" >>= get "pieces" >>= getString
    return $ fromIntegral (B.length pieces) `div` 20


infoPieceLength :: BCode -> Maybe Integer
infoPieceLength = get "info" >=> get "piece length" >=> getNumber


