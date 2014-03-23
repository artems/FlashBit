module FS
    ( TorrentFile
    , readBlock
    , writeBlock
    , checkPiece
    , checkTorrent
    , openTorrent
    , openAndCheckFile
    ) where

import Prelude hiding (readFile, writeFile)

import Control.Exception (catch, IOException)
import Control.Monad.State

import Data.Array
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import System.IO (Handle, SeekMode(AbsoluteSeek), IOMode(ReadWriteMode), hSeek, hFlush, openBinaryFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

import BCode (BCode)
import qualified BCode as BCode
import qualified BCodeTorrent as BCode
import Digest (digest)
import Torrent
import Peer
import Piece


newtype TorrentFile = TorrentFile [(Handle, Integer)]  -- ^ [(file handle, file length)]

projectFile :: TorrentFile
            -> Integer    -- ^ Torrent offset
            -> Integer    -- ^ Torrent size
            -- ^ (file handle, file chunk offset, file chunk size)
            -> [(Handle, Integer, Integer)]
projectFile (TorrentFile []) _ _ = []
projectFile (TorrentFile files@((handle, length'):files')) offset size
    | size <= 0 =
        fail "Попытка прочитать за пределами файла"
    | null files =
        fail "Попытка прочитать за пределами файла"
    | offset >= length' =
        projectFile (TorrentFile files') (offset - length') size
    | otherwise =
        let remain = length' - offset
         in if remain >= size
                then [(handle, offset, size)]
                else (handle, offset, remain) :
                    projectFile (TorrentFile files') 0 (size - remain)


readBlock :: TorrentFile -> PieceNum -> PieceBlock -> PieceArray -> IO B.ByteString
readBlock files pnum block pieces = do
    B.concat `fmap` forM fileMap readFile
  where
    piece = pieces ! pnum
    offset = pieceOffset piece + blockOffset block
    fileMap = projectFile files offset (blockSize block)
    readFile (handle, offset', size) = do
        hSeek handle AbsoluteSeek offset'
        B.hGet handle (fromInteger size)


writeBlock :: TorrentFile -> PieceNum -> PieceBlock -> PieceArray -> B.ByteString -> IO ()
writeBlock files pnum block pieces bs = do
    when lengthCheck $ fail "Попытка записать блок неверного размера"
    foldM_ writeFile bs fileMap
  where
    piece = pieces ! pnum
    offset = pieceOffset piece + blockOffset block
    length' = fromIntegral (B.length bs)
    fileMap = projectFile files offset length'
    lengthCheck = length' /= blockSize block
    writeFile acc (handle, offset', size) = do
        let (bs', rest) = B.splitAt (fromInteger size) acc
        hSeek handle AbsoluteSeek offset'
        B.hPut handle bs'
        hFlush handle
        return rest


checkPiece :: TorrentFile -> Piece -> IO Bool
checkPiece files piece = do
    bs <- B.concat `fmap` forM fileMap readFile
    return (pieceChecksum piece == digest bs)
  where
    fileMap = projectFile files (pieceOffset piece) (pieceLength piece)
    readFile (handle, offset, size) = do
        hSeek handle AbsoluteSeek offset
        B.hGet handle (fromInteger size)


checkTorrent:: TorrentFile -> PieceArray -> IO PieceHaveMap
checkTorrent files pieces = do
    M.fromList `fmap` mapM mymap (assocs pieces)
  where
    mymap (pnum, piece) = do
        isValid <- checkPiece files piece
        return (pnum, isValid)


openTorrent :: FilePath -> IO (Either String BCode)
openTorrent filepath = do
    fileAttempt <- catch
        (Right `fmap` B.readFile filepath)
        (\e -> return . Left . show $ (e :: IOException))
    case fileAttempt of
        Left msg -> failure msg
        Right fileCoded ->
            case BCode.decode fileCoded of
                Left msg -> failure msg
                Right bc -> return (Right bc)
  where
    failure msg = return . Left $ "Внутреняя ошибка: " ++ show msg


openAndCheckFile :: BCode -> IO (TorrentFile, PieceArray, PieceHaveMap)
openAndCheckFile bc = do
    targets <- TorrentFile `fmap` forM files openFile
    pieceHaveMap <- checkTorrent targets pieceArray
    return (targets, pieceArray, pieceHaveMap)
  where
    Just files = BCode.infoFiles bc
    Just pieceArray = mkPieceArray bc
    openFile (path, length') = do
        let dir = joinPath (init path)
            filepath = joinPath path
        when (dir /= "") $ createDirectoryIfMissing True dir
        handle <- openBinaryFile filepath ReadWriteMode
        return (handle, length')


