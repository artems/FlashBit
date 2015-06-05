{-# LANGUAGE DeriveDataTypeable #-}

module Torrent.Exception
    ( TorrentException(..)
    ) where
 
import Data.Typeable
import Control.Exception

data TorrentException
    = TorrentCorrupted String
    | TorrentMalformed String
    | TorrentException String
    deriving (Show, Typeable)

instance Exception TorrentException


