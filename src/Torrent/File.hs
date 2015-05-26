module Torrent.File
    ( FileRec(..)
    , readBlock
    , writeBlock
    , checkPiece
    , openTorrent
    , checkTorrent
    , openTarget
    , bytesLeft
    ) where

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (foldl')
import qualified Data.Map as M
import Control.Exception (catch, IOException)
import Control.Monad (forM, when, foldM_)
import qualified Crypto.Hash.SHA1 as SHA1

import System.IO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

import Torrent
import Torrent.BCode (BCode)
import qualified Torrent.BCode as BCode
import qualified Torrent.Metafile as BCode


-- TODO
--  * заменить error'ы на исключения

newtype FileRec = FileRec [(Handle, Integer)]  -- ^ [(file handle, file size)]

projectFile :: FileRec
            -- ^ offset
            -> Integer
            -- ^ size
            -> Integer
            -- ^ (file handle, file chunk offset, file chunk size)
            -> [(Handle, Integer, Integer)]
projectFile (FileRec []) _ _ = error "Попытка прочитать за пределами файла"
projectFile (FileRec ((handle, filesize) : files)) offset size
    | offset >= filesize
        = projectFile (FileRec files) (offset - filesize) size
    | otherwise =
        let remain = filesize - offset
        in if remain >= size
                then [(handle, offset, size)]
                else (handle, offset, remain) : projectFile (FileRec files) 0 (size - remain)

readChunk :: (Handle, Integer, Integer) -> IO B.ByteString
readChunk (handle, offset, size) = do
    hSeek handle AbsoluteSeek offset
    B.hGet handle (fromInteger size)

readBlock :: FileRec -> PieceRec -> PieceBlock -> IO B.ByteString
readBlock file piece block = do
    B.concat `fmap` forM fileMap readChunk
  where
    offset = _pieceOffset piece + _blockOffset block
    fileMap = projectFile file offset (_blockLength block)

writeBlock :: FileRec -> PieceRec -> PieceBlock -> B.ByteString -> IO ()
writeBlock file piece block bs = do
    when sizeCheck $ error "Torrent.File.writeBlock: попытка записать больше, чем размер блока"
    foldM_ writeFile' bs fileMap
  where
    bsSize = fromIntegral (B.length bs)
    offset = _pieceOffset piece + _blockOffset block
    sizeCheck = bsSize /= _blockLength block

    fileMap = projectFile file offset bsSize
    writeFile' acc (handle, offset', size) = do
        let (bs', rest) = B.splitAt (fromInteger size) acc
        hSeek handle AbsoluteSeek offset'
        B.hPut handle bs'
        hFlush handle
        return rest

checkPiece :: FileRec -> PieceRec -> IO Bool
checkPiece file piece = do
    bs <- B.concat `fmap` forM fileMap readChunk
    return (_pieceChecksum piece == SHA1.hash bs)
  where
    fileMap = projectFile file (_pieceOffset piece) (_pieceLength piece)

checkTorrent :: FileRec -> PieceArray -> IO PieceHaveMap
checkTorrent file pieceArray = do
    M.fromList `fmap` mapM checkPiece' (A.assocs pieceArray)
  where
    checkPiece' (pieceNum, piece) = do
        isValid <- checkPiece file piece
        return (pieceNum, isValid)

openTorrent :: FilePath -> IO BCode
openTorrent filepath = do
    openAttempt <- catch
        (Right `fmap` B.readFile filepath)
        (\e -> return . Left . show $ (e :: IOException))
    case openAttempt of
        Left msg ->
            error $ "Torrent.File.openTorrent: Ошибка при открытии файла: " ++ msg
        Right fileCoded ->
            case BCode.decode fileCoded of
                Left msg -> error $ "Torret.File.openTorrent: Ошибка при чтении файла: " ++ show msg
                Right bc -> return bc

openTarget :: FilePath -> BCode -> IO (FileRec, PieceArray, PieceHaveMap)
openTarget prefix bc = do
    files <- whenNothing (BCode.infoFiles bc) $
        error "Torrent.File.openTarget: Ошибка при чтении файла. Файл поврежден (1)"
    pieceArray <- whenNothing (mkPieceArray bc) $
        error "Torrent.File.openTarget: Ошибка при чтении файла. Файл поврежден (2)"
    target <- FileRec `fmap` forM files openTargetFile
    pieceHaveMap <- checkTorrent target pieceArray
    return (target, pieceArray, pieceHaveMap)
  where
    whenNothing (Just a) _     = return a
    whenNothing Nothing action = action

    openTargetFile :: ([B.ByteString], Integer) -> IO (Handle, Integer)
    openTargetFile (pathCoded, filelen) = do
        let path = prefix : map B8.unpack pathCoded -- TODO decode using encoding
            targetDir = joinPath (init path)
            targetPath = joinPath path
        when (targetDir /= "") $ createDirectoryIfMissing True targetDir
        handle <- openBinaryFile targetPath ReadWriteMode
        return (handle, filelen)

bytesLeft :: PieceArray -> PieceHaveMap -> Integer
bytesLeft pieces haveMap = foldl' f 0 (A.assocs pieces)
  where
    f acc (pieceNum, piece) =
        case M.lookup pieceNum haveMap of
            Just False -> (_pieceLength piece) + acc
            _          -> acc
