{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty

-- import qualified Data.Queue.Test
import qualified Data.PieceSet.Test
import qualified URI.Test
import qualified Rate.Test
import qualified Torrent.Test
import qualified Torrent.File.Test
import qualified Torrent.BCode.Test
import qualified Torrent.Message.Test
import qualified Torrent.Metafile.Test
import qualified Torrent.Tracker.Test
import qualified Torrent.Announce.Test


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "(all)"
    -- [ Data.Queue.Test.tests
    [ Data.PieceSet.Test.tests
    , URI.Test.tests
    , Rate.Test.tests
    , Torrent.Test.tests
    , Torrent.BCode.Test.tests
    , Torrent.Message.Test.tests
    , Torrent.Metafile.Test.tests
    , Torrent.Tracker.Test.tests
    , Torrent.File.Test.tests
    , Torrent.Announce.Test.tests
    ]
