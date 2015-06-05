{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Torrent.File
    ( FileRec(..)
    , readBlock
    , writeBlock
    , checkPiece
    , openTorrent
    , openMetafile
    , checkTorrent
    , openTarget
    , torrentBytesLeft
    ) where

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Data.List (foldl')
import qualified Data.Map as M
import           Control.Monad (forM, when, foldM_)
import           Control.Exception hiding (handle)
import           System.IO
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (joinPath)

import           Torrent
import           Torrent.BCode (BCode)
import qualified Torrent.BCode as BCode
import qualified Crypto.Hash.SHA1 as SHA1


newtype FileRec = FileRec [(Handle, Integer)]  -- ^ [(file handle, file size)]

projectFile :: FileRec -> Integer -> Integer -> [(Handle, Integer, Integer)]
projectFile (FileRec []) _ _ = throw $ TorrentException "projectFile: offset or size too long"
projectFile (FileRec ((handle, filesize) : files)) offset size
    | offset >= filesize = projectFile (FileRec files) (offset - filesize) size
    | size > remain      = (handle, offset, remain) : nextFile
    | otherwise          = [(handle, offset, size)]
  where
    remain = filesize - offset
    nextFile = projectFile (FileRec files) 0 (size - remain)

readChunk :: (Handle, Integer, Integer) -> IO B.ByteString
readChunk (handle, offset, size) = do
    hSeek handle AbsoluteSeek offset
    B.hGet handle (fromInteger size)

readBlock :: FileRec -> PieceRec -> PieceBlock -> IO B.ByteString
readBlock file piece block = do
    B.concat `fmap` forM target readChunk
  where
    offset = _pieceOffset piece + _blockOffset block
    target = projectFile file offset (_blockLength block)

writeBlock :: FileRec -> PieceRec -> PieceBlock -> B.ByteString -> IO ()
writeBlock file piece block bs
    | sizeCheck = throwIO . TorrentException $ "writeBlock: bytesting too long"
    | otherwise = foldM_ writeFile' bs target
  where
    offset    = _pieceOffset piece + _blockOffset block
    bsSize    = fromIntegral (B.length bs)
    target    = projectFile file offset bsSize
    sizeCheck = bsSize /= _blockLength block
    writeFile' acc (handle, offset', size) = do
        let (bs', rest) = B.splitAt (fromInteger size) acc
        hSeek handle AbsoluteSeek offset'
        B.hPut handle bs'
        hFlush handle
        return rest

checkPiece :: FileRec -> PieceRec -> IO Bool
checkPiece file piece = do
    bs <- B.concat `fmap` forM target readChunk
    return (_pieceChecksum piece == SHA1.hash bs)
  where
    target = projectFile file (_pieceOffset piece) (_pieceLength piece)

checkTorrent :: FileRec -> PieceArray -> IO PieceHaveMap
checkTorrent file pieceArray = do
    M.fromList `fmap` mapM checkPiece' (A.assocs pieceArray)
  where
    checkPiece' (pieceNum, piece) = do
        !have <- checkPiece file piece
        return (pieceNum, have)

openTorrent :: FilePath -> IO Torrent
openTorrent filepath = do
    bc <- openMetafile filepath
    case mkTorrent bc of
        Just torrent -> return torrent
        Nothing      -> throwIO . TorrentMalformed $ "unknown"

openMetafile :: FilePath -> IO BCode
openMetafile filepath = do
    fileCoded <- B.readFile filepath
    case BCode.decode fileCoded of
        Left msg -> throwIO . TorrentCorrupted $ show msg
        Right bc -> return bc

openTarget :: Torrent -> FilePath -> IO (FileRec, PieceHaveMap)
openTarget torrent prefix = do
    target <- FileRec `fmap` forM files (openTargetFile prefix)
    pieceHaveMap <- checkTorrent target (_torrentPieceArray torrent)
    return (target, pieceHaveMap)
  where
    files = _torrentInfoFiles torrent

openTargetFile :: FilePath -> ([B.ByteString], Integer) -> IO (Handle, Integer)
openTargetFile prefix (pathCoded, filelen) = do
    let path = prefix : map B8.unpack pathCoded -- TODO decode utf-8
        targetDir = joinPath (init path)
        targetPath = joinPath path
    when (targetDir /= "") $
        createDirectoryIfMissing True targetDir
    handle <- openBinaryFile targetPath ReadWriteMode
    return (handle, filelen)

torrentBytesLeft :: PieceArray -> PieceHaveMap -> Integer
torrentBytesLeft pieces haveMap = foldl' f 0 (A.assocs pieces)
  where
    f acc (pieceNum, piece) =
        case M.lookup pieceNum haveMap of
            Just False -> (_pieceLength piece) + acc
            _          -> acc
