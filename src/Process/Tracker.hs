{-# LANGUAGE FlexibleInstances #-}

module Process.Tracker
    ( runTracker
    , TrackerMessage(..)
    ) where


import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Word

import Network.HTTP hiding (urlEncodeVars)
import Network.Stream (ConnError (ErrorMisc))
import qualified Network.Socket as S
import Network.URI

import URI (urlEncodeVars)
import Timer
import Torrent
import Protocol

import BCode (BCode)
import qualified BCode as BCode
import qualified BCodeTracker as BCode

import Process
import qualified Process.Status as Status

import Platform.Process


data PConf = PConf
    { cInfoHash :: InfoHash
    , cStatusChan :: TChan Status.StatusMessage
    , cTrackerChan :: TChan TrackerMessage
    }

data PState = PState
    { sPeerId :: PeerId
    , sTorrent :: Torrent
    , sAnnounceList :: [[B.ByteString]]
    , sStatus :: TrackerEvent
    , sLocalPort :: Word16
    , sNextTick :: Integer
    }

instance ProcessName PConf where
    processName _ = "Tracker"


data TrackerMessage
    = Stop                  -- ^ Сообщить трекеру об остановки скачивания
    | Start                 -- ^ Сообщить трекеру о начале скачивания
    | Complete              -- ^ Сообщить трекеру об окончании скачивания
    | TrackerTick Integer   -- ^ ?
    | Terminate


data TrackerEvent = Started | Stopped | Completed | Running
    deriving (Show, Eq)


data TrackerResponse
    = ResponseNormal
        { rPeer :: [Peer]
        , rComplete :: Maybe Integer
        , rIncomplete :: Maybe Integer
        , rTimeoutInterval :: Integer
        , rTimeoutMinInterval :: Maybe Integer
        }
    | ResponseDecodeError B.ByteString
    | ResponseWarning B.ByteString
    | ResponseError B.ByteString


failTimerInterval :: Integer
failTimerInterval = 15 * 60


runTracker :: PeerId -> Torrent -> Word16 -> TChan Status.StatusMessage -> TChan TrackerMessage
           -> IO Reason
runTracker peerId torrent port' statusChan trackerChan =
    process1 "Tracker" conf state' wait receive terminate
  where
    conf = let
        infoHash = torrentInfoHash torrent
        in PConf infoHash statusChan trackerChan
    state' = let
        announceURL = torrentAnnounceURL torrent
        in PState peerId torrent announceURL Stopped port' 0
    wait = liftIO . atomically $ readTChan trackerChan
    receive msg = handleMessage msg


terminate :: Reason -> Process PConf PState ()
terminate _reason = modify (\s -> s { sStatus = Stopped }) >> talkTracker


handleMessage :: TrackerMessage -> Process PConf PState ()
handleMessage msg = case msg of
    Stop ->
        modify (\s -> s { sStatus = Stopped }) >> talkTracker
    Start ->
        modify (\s -> s { sStatus = Started }) >> talkTracker
    Complete ->
        modify (\s -> s { sStatus = Completed }) >> talkTracker
    TrackerTick x -> do
        tick <- gets sNextTick
        when (x == tick) talkTracker
    Terminate ->
        stopProcess Shutdown


talkTracker :: Process PConf PState ()
talkTracker = pokeTracker >>= timerUpdate


pokeTracker :: Process PConf PState (Integer, Maybe Integer)
pokeTracker = do
    infoHash <- asks cInfoHash
    statusChan <- asks cStatusChan
    tStat <- torrentStatus infoHash statusChan
    response <- queryTracker tStat
    case response of
        Left msg -> do
            infoP $ "Request error: " ++ msg
            return (failTimerInterval, Nothing)
        Right (ResponseError msg) -> do
            infoP $ "Tracker error: " ++ B8.unpack msg
            return (failTimerInterval, Nothing)
        Right (ResponseWarning msg) -> do
            infoP $ "Tracker warning: " ++ B8.unpack msg
            return (failTimerInterval, Nothing)
        Right (ResponseDecodeError msg) -> do
            infoP $ "Response decode error: " ++ B8.unpack msg
            return (failTimerInterval, Nothing)
        Right rsp -> do
            trackerStat infoHash statusChan rsp
            eventTransition
            return (rTimeoutInterval rsp, rTimeoutMinInterval rsp)
  where
    trackerStat infoHash statusChan rsp = do
        let trackerStat
                = Status.TrackerStat
                { Status.trackInfoHash = infoHash
                , Status.trackComplete = rComplete rsp
                , Status.trackIncomplete = rIncomplete rsp
                }
        liftIO . atomically $ writeTChan statusChan trackerStat

    torrentStatus infoHash statusChan = do
        statusTV <- liftIO newEmptyTMVarIO
        liftIO . atomically $ writeTChan statusChan (Status.RequestStatus infoHash statusTV)
        liftIO . atomically $ takeTMVar statusTV


eventTransition :: Process PConf PState ()
eventTransition = do
    status <- gets sStatus
    modify (\s -> s { sStatus = newStatus status })
  where
    newStatus status = case status of
        Started -> Running
        Stopped -> Stopped
        Running -> Running
        Completed -> Running


timerUpdate :: (Integer, Maybe Integer) -> Process PConf PState ()
timerUpdate (timeout, _minTimeout) = do
    status <- gets sStatus
    if status /= Running
        then return ()
        else do
            tick <- gets sNextTick
            chan <- asks cTrackerChan
            let nextTick = tick + 1
            modify (\s -> s { sNextTick = nextTick })
            _ <- liftIO $ setTimeout (fromIntegral timeout) $
                atomically $ writeTChan chan (TrackerTick nextTick)
            debugP $ "Установлен таймаут обращения к трекеру: " ++ show timeout


queryTracker
    :: Status.StatusState
    -> Process PConf PState (Either String TrackerResponse)
queryTracker torrentStatus = do
    announceList <- gets sAnnounceList
    queryTracker' torrentStatus announceList


queryTracker'
    :: Status.StatusState
    -> [[B.ByteString]]
    -> Process PConf PState (Either String TrackerResponse)
queryTracker' _torrentStatus [] = return $ Left "Пустой список трекеров"
queryTracker' torrentStatus (x:[]) = tryTier torrentStatus x
queryTracker' torrentStatus (x:xs) = do
    response <- tryTier torrentStatus x
    case response of
        Left _ -> queryTracker' torrentStatus xs
        Right _ -> return response


tryTier
    :: Status.StatusState
    -> [B.ByteString]
    -> Process PConf PState (Either String TrackerResponse)
tryTier torrentStatus tier = do
    response <- tryTier' torrentStatus tier
    case response of
        Left e -> return (Left e)
        Right (url, response') -> do
            bubble url tier
            return (Right response')


tryTier'
    :: Status.StatusState
    -> [B.ByteString]
    -> Process PConf PState (Either String (B.ByteString, TrackerResponse))
tryTier' _torrentStatus [] = return (Left "Пустой список трекеров")
tryTier' torrentStatus (trackerUrl:xs) = do
    url <- buildRequest torrentStatus trackerUrl
    uri <- case parseURI url of
        Just u -> return u
        Nothing -> fail $ "Не удается распознать адрес " ++ url
    response <- trackerRequest uri
    case response of
        Left m ->
            if null xs
                then return (Left m)
                else tryTier' torrentStatus xs
        Right response' ->
            return (Right (trackerUrl, response'))


trackerRequest :: URI -> Process PConf PState (Either String TrackerResponse)
trackerRequest uri = do
    debugP $ "Отправка запроса: " ++ show uri
    response <- liftIO $ catch (simpleHTTP request) catchError
    case response of
        Left x -> do
            let err = "Ошибка соединения: " ++ show x
            debugP err >> return (Left err)
        Right r ->
            case rspCode r of
                (2, _, _) ->
                    case BCode.decode (rspBody r) of
                        Left e -> do
                            let err = "Не удается разобрать ответ от трекера: " ++ show e
                            debugP err >> return (Left err)
                        Right bc -> do
                            -- debugP $ "Response: " ++ BCode.prettyPrint bc
                            return (Right (processResult bc))
                (3, _, _) ->
                    case findHeader HdrLocation r of
                        Just uri' ->
                            case parseURI uri' of
                                Just newURI -> trackerRequest newURI
                                Nothing -> do
                                    let err = "Не удается распознать адрес " ++ uri'
                                    debugP err >> return (Left err)
                        Nothing -> do
                            let err = "В ответе от трекера отсутвует заголовок Location"
                            debugP err >> return (Left err)
                _ -> do
                    let err = "Трекер не доступен: " ++ show r
                    debugP err >> return (Left err)
  where
    request = Request
        { rqURI = uri
        , rqMethod = GET
        , rqHeaders = []
        , rqBody = B.empty
        }
    catchError e = return (Left . ErrorMisc $ show (e :: IOException))


bubble :: B.ByteString -> [B.ByteString] -> Process PConf PState ()
bubble _ [] = return ()
bubble _ (_:[]) = return ()
bubble url tier@(x:_) =
    if url == x
        then return ()
        else do
            announceList <- gets sAnnounceList
            let newTier = url : filter (/= url) tier
                newAnnounceList = map (\a -> if a == tier then newTier else a) announceList
            modify (\s -> s { sAnnounceList = newAnnounceList })
            return ()


buildRequest :: Status.StatusState -> B.ByteString -> Process PConf PState String
buildRequest torrentStatus url = do
    params <- urlEncodeVars `fmap` buildRequestParams torrentStatus
    let announce = B8.unpack url
        separator = if '?' `elem` announce then "&" else "?"
    return $ concat [announce, separator, params]


buildRequestParams :: Status.StatusState -> Process PConf PState [(B.ByteString, B.ByteString)]
buildRequestParams torrentStatus = do
    s <- get
    return $
        [
            (B8.pack "port", B8.pack . show $ sLocalPort s),
            (B8.pack "peer_id", B8.pack $ sPeerId s),
            (B8.pack "info_hash", torrentInfoHash $ sTorrent s),
            (B8.pack "left", B8.pack . show $ Status.sLeft torrentStatus),
            (B8.pack "uploaded", B8.pack . show $ Status.sUploaded torrentStatus),
            (B8.pack "downloaded", B8.pack . show $ Status.sDownloaded torrentStatus),
            (B8.pack "compact", B8.pack "1")
        ] ++ (event $ sStatus s)
  where
    event e = case e of
        Running -> []
        Stopped -> [(B8.pack "event", B8.pack "stopped")]
        Started -> [(B8.pack "event", B8.pack "started")]
        Completed -> [(B8.pack "event", B8.pack "completed")]


processResult :: BCode -> TrackerResponse
processResult bc =
    case BCode.trackerError bc of
        Just e -> ResponseError e
        Nothing -> case BCode.trackerWarning bc of
            Just w -> ResponseWarning w
            Nothing -> case decode of
                Just ok -> ok
                Nothing -> ResponseDecodeError $ B8.pack "Не удалось распознать ответ от сервера"
  where
    decode = ResponseNormal
        <$> (decodeIp <$> BCode.trackerPeers bc)
        <*> pure (BCode.trackerComplete bc)
        <*> pure (BCode.trackerIncomplete bc)
        <*> BCode.trackerInterval bc
        <*> pure (BCode.trackerMinInterval bc)


decodeIp :: (B.ByteString, B.ByteString) -> [Peer]
decodeIp (ipv4, ipv6) = decodeIp4 ipv4 ++ decodeIp6 ipv6

decodeIp4 :: B.ByteString -> [Peer]
decodeIp4 bs
    | B.null bs = []
    | B.length bs >= 6 = let
        (peer, remain) = B.splitAt 6 bs
        (ipCoded, portCoded) = B.splitAt 4 peer
        ip = cw32 ipCoded
        port' = S.PortNum (cw16 portCoded)
        in (Peer $ S.SockAddrInet port' ip) : decodeIp4 remain
    | otherwise = []

decodeIp6 :: B.ByteString -> [Peer]
decodeIp6 bs
    | B.null bs = []
    | B.length bs >= 18 = let
        (peer, remain) = B.splitAt 18 bs
        (ipCoded, portCoded) = B.splitAt 16 peer
        ip = cW128 ipCoded
        port' = S.PortNum (cw16 portCoded)
        in (Peer $ S.SockAddrInet6 port' 0 ip 0) : decodeIp6 remain
    | otherwise = []


cw16 :: B.ByteString -> Word16
cw16 bs = foldr (\w8 acc -> acc `shiftL` 8 + fromIntegral w8) 0 (B.unpack bs)

cw32 :: B.ByteString -> Word32
cw32 bs = foldr (\w8 acc -> acc `shiftL` 8 + fromIntegral w8) 0 (B.unpack bs)

cW128 :: B.ByteString -> (Word32, Word32, Word32, Word32)
cW128 bs = let
    (q1, r1) = B.splitAt 4 bs
    (q2, r2) = B.splitAt 4 r1
    (q3, q4) = B.splitAt 4 r2
    in (cw32 q1, cw32 q2, cw32 q3, cw32 q4)


