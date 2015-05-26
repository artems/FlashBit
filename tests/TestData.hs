module TestData
    ( withTorrentFile
    ) where

import qualified Data.ByteString as B
import qualified Torrent.BCode as BCode

withTorrentFile :: FilePath -> (BCode.BCode -> IO a) -> IO a
withTorrentFile torrent action = setUp >>= action
  where
    setUp = do
        content <- B.readFile torrent
        let (Right bc) = BCode.decode content
        return bc
