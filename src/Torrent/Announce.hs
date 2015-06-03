{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Torrent.Announce
    ( TrackerParam(..)
    , TrackerStatus(..)
    , TrackerResponse(..)
    , TrackerResponseError(..)
    , askTracker
    , buildRequest
    , bubbleAnnounce
    , trackerRequest
    , parseResponse
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Dynamic
import Data.Word (Word16)

import Control.Applicative
import Control.Exception

import Network.HTTP hiding (urlEncodeVars)
import Network.Stream (ConnError (ErrorMisc))
import Network.URI.Extra (URI(..), parseURI, urlEncodeVars)

import Torrent
import Torrent.BCode (BCode)
import qualified Torrent.BCode as BCode
import qualified Torrent.Tracker as BCode


data TrackerParam = TrackerParam
    { _paramPeerId      :: String
    , _paramInfoHash    :: B.ByteString
    , _paramLocalPort   :: Word16
    , _paramLeft        :: Integer
    , _paramUploaded    :: Integer
    , _paramDownloaded  :: Integer
    , _paramStatus      :: TrackerStatus
    }

data TrackerStatus
    = Running
    | Stopped
    | Started
    | Completed
    deriving (Eq, Show)

data TrackerResponse
    = TrackerResponse
        { _trackerPeers       :: [Peer]
        , _trackerComplete    :: Maybe Integer
        , _trackerIncomplete  :: Maybe Integer
        , _trackerInterval    :: Integer
        , _trackerMinInterval :: Maybe Integer
        }
    deriving (Eq, Show)

data TrackerResponseError
    = TrackerReturnError String
    | TrackerReturnWarning String
    | TrackerNetworkError String
    | TrackerDecodeError String
    | TrackerParseError String
    | TrackerConnError String
    | TrackerMiscError String
    deriving (Show, Typeable)

instance Exception TrackerResponseError

bubbleAnnounce
    :: B.ByteString
    -> [B.ByteString]
    -> AnnounceList
    -> AnnounceList
bubbleAnnounce _   []           announce = announce
bubbleAnnounce _   (_ : [])     announce = announce
bubbleAnnounce url tier@(x : _) announce
    | x == url  = announce
    | otherwise = map updateTier announce
  where
    updateTier tier'
        | tier == tier' = url : filter (/= url) tier
        | otherwise     = tier'

buildRequest :: String -> TrackerParam -> String
buildRequest url params =
    let query = buildRequestParams params
        separator = if '?' `elem` url then "&" else "?"
    in concat [url, separator, urlEncodeVars query]

buildRequestParams :: TrackerParam -> [(B.ByteString, B.ByteString)]
buildRequestParams params =
    [ (B8.pack "num", packNum (50 :: Int))
    , (B8.pack "port", packNum $ _paramLocalPort params)
    , (B8.pack "compact", packNum (1 :: Int))
    , (B8.pack "peer_id", B8.pack $ _paramPeerId params)
    , (B8.pack "info_hash", _paramInfoHash params)
    , (B8.pack "left", packNum $ _paramLeft params)
    , (B8.pack "uploaded", packNum $ _paramUploaded params)
    , (B8.pack "downloaded", packNum $ _paramDownloaded params)
    ] ++ (event $ _paramStatus params)
  where
    event e = case e of
        Running   -> []
        Stopped   -> [(B8.pack "event", B8.pack "stopped")]
        Started   -> [(B8.pack "event", B8.pack "started")]
        Completed -> [(B8.pack "event", B8.pack "completed")]

    packNum :: (Show a, Integral a) => a -> B.ByteString
    packNum = B8.pack . show

askTracker
    :: TrackerParam
    -> AnnounceList
    -> IO (AnnounceList, TrackerResponse)
askTracker params announce = do
    (url, tier, rsp) <- queryTracker params announce
    return (bubbleAnnounce url tier announce, rsp)

queryTracker
    :: TrackerParam
    -> AnnounceList
    -> IO (B.ByteString, [B.ByteString], TrackerResponse)
queryTracker _ [] =
    throw . TrackerMiscError $ "Список трекеров пуст (1)"
queryTracker params (tier : xs) = do
    result <- try $ tryTier params tier
    case result of
        Left (err :: TrackerResponseError)
            | null xs    -> throw err
            | otherwise  -> queryTracker params xs
        Right (url, rsp) -> return (url, tier, rsp)

tryTier :: TrackerParam
        -> [B.ByteString]
        -> IO (B.ByteString, TrackerResponse)
tryTier _ [] =
    throw . TrackerMiscError $ "Список трекеров пуст (2)"
tryTier params (url : xs) = do
    let urlBuilded = buildRequest (B8.unpack url) params
    case parseURI urlBuilded of
        Just urlParsed -> do
            result <- try $ trackerRequest urlParsed
            case result of
                Left (err :: TrackerResponseError)
                    | null xs   -> throw err
                    | otherwise -> tryTier params xs
                Right rsp       -> return (url, rsp)
        Nothing -> throw . TrackerMiscError $ "Неправильный url: " ++ urlBuilded

trackerRequest :: URI -> IO TrackerResponse
trackerRequest uri = do
    response <- catch (simpleHTTP request) catchError
    case response of
        Left err -> throw . TrackerConnError $ show err
        Right rsp -> case rspCode rsp of
            (2, _, _) -> case BCode.decode (rspBody rsp) of
                Left msg -> throw . TrackerParseError $ show msg
                Right bc -> return $ parseResponse bc
            (_, _, _) -> do
                throw . TrackerNetworkError $ show rsp
  where
    request = Request
        { rqURI     = uri
        , rqBody    = B.empty
        , rqMethod  = GET
        , rqHeaders = []
        }
    catchError e = return . Left . ErrorMisc $ show (e :: IOException)

parseResponse :: BCode -> TrackerResponse
parseResponse bc =
    case BCode.trackerError bc of
        Just e -> throw . TrackerReturnError . B8.unpack $ e -- TODO utf-8 decode
        Nothing -> case BCode.trackerWarning bc of
            Just w -> throw . TrackerReturnWarning . B8.unpack $ w -- TODO utf-8 decode
            Nothing -> case decode bc of
                Just ok -> ok
                Nothing -> throw . TrackerDecodeError $ show bc
  where
    decode msg = TrackerResponse
        <$> (map Peer `fmap` BCode.trackerPeers msg)
        <*> pure (BCode.trackerComplete msg)
        <*> pure (BCode.trackerIncomplete msg)
        <*> BCode.trackerInterval msg
        <*> pure (BCode.trackerMinInterval msg)
