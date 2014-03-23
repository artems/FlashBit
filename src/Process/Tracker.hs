module Process.Tracker
    ( runTracker
    ) where


import Control.Applicative
import Control.Concurrent
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
import Torrent.BCode (BCode)
import qualified Torrent.BCode as BCode
import Process
import Process.Status
import Process.PeerManager


data TrackerMessage
    = TrackerStop           -- ^ Сообщить трекеру об остановки скачивания
    | TrackerStart          -- ^ Сообщить трекеру о начале скачивания
    | TrackerComplete       -- ^ Сообщить трекеру об окончании скачивания
    | TrackerTick Integer   -- ^ ?


data PConf = PConf
    { _infoHash :: InfoHash
    , _peerMChan :: TChan PeerManagerMessage
    , _statusChan :: TChan StatusMessage
    , _trackerChan :: TChan TrackerMessage
    }

instance ProcessName PConf where
    processName _ = "Tracker"


data PState = PState
    { _peerId :: PeerId
    , _torrent :: Torrent
    , _announceList :: [[B.ByteString]]
    , _trackerStatus :: TrackerEvent
    , _localPort :: Word16
    , _nextTick :: Integer
    }


data TrackerEvent = Started | Stopped | Completed | Running
    deriving (Eq, Show)


data TrackerResponse
    = ResponseNormal
        { _trackerPeers       :: [Peer]
        , _torrentComplete    :: Maybe Integer
        , _torrentIncomplete  :: Maybe Integer
        , _timeoutInterval    :: Integer
        , _timeoutMinInterval :: Maybe Integer
        }
    | ResponseDecodeError B.ByteString
    | ResponseWarning B.ByteString
    | ResponseError B.ByteString


failTimerInterval :: Integer
failTimerInterval = 15 * 60


runTracker :: PeerId -> InfoHash -> Torrent -> Word16
    -> TChan TrackerMessage
    -> TChan StatusMessage
    -> TChan PeerManagerMessage
    -> IO ()
runTracker peerId infohash torrent port trackerChan statusChan peerMChan = do
    let infohash    = _torrentInfoHash torrent
        announceURL = _torrentAnnounceURL torrent
    let pconf  = PConf infohash peerMChan statusChan trackerChan
        pstate = PState peerId torrent announceURL Stopped port 0
    wrapProcess pconf pstate process


process :: Process PConf PState ()
process = do
    message <- wait
    receive message
    process


wait :: Process PConf PState TrackerMessage
wait = do
    trackerChan <- asks _trackerChan
    liftIO . atomically $ readTChan trackerChan


receive :: TrackerMessage -> Process PConf PState ()
receive message = do
    case message of
        TrackerStop ->
            modify (\s -> s { _trackerStatus = Stopped }) >> talkTracker
        TrackerStart ->
            modify (\s -> s { _trackerStatus = Started }) >> talkTracker
        TrackerComplete ->
            modify (\s -> s { _trackerStatus = Completed }) >> talkTracker
        TrackerTick x -> do
            tick <- gets _nextTick
            when (x == tick) talkTracker
  where
    talkTracker = pokeTracker >>= timerUpdate


pokeTracker :: Process PConf PState (Integer, Maybe Integer)
pokeTracker = do
    status   <- getTorrentStatus
    response <- queryTracker status
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
            eventTransition
            return (_timeoutInterval rsp, _timeoutMinInterval rsp)
  where
    getTorrentStatus = do
        statusV    <- liftIO newEmptyTMVarIO
        infohash   <- asks _infoHash
        statusChan <- asks _statusChan
        liftIO . atomically $ writeTChan statusChan $ RequestStatus infohash statusV
        liftIO . atomically $ takeTMVar statusV


eventTransition :: Process PConf PState ()
eventTransition = do
    status <- gets _trackerStatus
    modify $ \st -> st { _trackerStatus = newStatus status }
  where
    newStatus status = case status of
        Started -> Running
        Stopped -> Stopped
        Running -> Running
        Completed -> Running


timerUpdate :: (Integer, Maybe Integer) -> Process PConf PState ()
timerUpdate (timeout, _minTimeout) = do
    status <- gets _trackerStatus
    if status /= Running
        then return ()
        else do
            tick <- gets _nextTick
            chan <- asks _trackerChan
            let nextTick = tick + 1
            modify (\s -> s { _nextTick = nextTick })
            liftIO $ setTimeout (fromIntegral timeout) $
                atomically $ writeTChan chan (TrackerTick nextTick)
            debugP $ "Установлен таймаут обращения к трекеру: " ++ show timeout


queryTracker
    :: StatusState
    -> Process PConf PState (Either String TrackerResponse)
queryTracker torrentStatus = do
    announceList <- gets _announceList
    queryTracker' torrentStatus announceList


queryTracker'
    :: StatusState
    -> [[B.ByteString]]
    -> Process PConf PState (Either String TrackerResponse)
queryTracker' torrentStatus []     = return . Left $ "Пустой список трекеров"
queryTracker' torrentStatus (x:[]) = tryTier torrentStatus x
queryTracker' torrentStatus (x:xs) = do
    response <- tryTier torrentStatus x
    case response of
        Left _  -> queryTracker' torrentStatus xs
        Right _ -> return response


tryTier
    :: StatusState
    -> [B.ByteString]
    -> Process PConf PState (Either String TrackerResponse)
tryTier torrentStatus tier = do
    response <- tryTier' torrentStatus tier
    case response of
        Left e ->
            return . Left $ e
        Right (url, response) -> do
            bubble url tier
            return . Right $ response


tryTier'
    :: StatusState
    -> [B.ByteString]
    -> Process PConf PState (Either String (B.ByteString, TrackerResponse))
tryTier' _             []                =
    return (Left "Пустой список трекеров")
tryTier' torrentStatus (trackerUrl : xs) = do
    url <- buildRequest torrentStatus trackerUrl
    uri <- case parseURI url of
        Just u  -> return u
        Nothing -> fail $ "Не удается распознать адрес " ++ url
    response <- trackerRequest uri
    case response of
        Left m ->
            if null xs
                then return (Left m)
                else tryTier' torrentStatus xs
        Right response ->
            return . Right $ (trackerUrl, response)


trackerRequest :: URI -> Process PConf PState (Either String TrackerResponse)
trackerRequest uri = do
    debugP $ "Запрос: " ++ show uri
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
                            -- debugP $ "Ответ: " ++ BCode.prettyPrint bc
                            return . Right $ processResult bc
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
        { rqURI     = uri
        , rqMethod  = GET
        , rqHeaders = []
        , rqBody    = B.empty
        }
    catchError e = return (Left . ErrorMisc $ show (e :: IOException))


bubble :: B.ByteString -> [B.ByteString] -> Process PConf PState ()
bubble _ [] = return ()
bubble _ (_:[]) = return ()
bubble url tier@(x:_) =
    if url == x
        then return ()
        else do
            announceList <- gets _announceList
            let newTier = url : filter (/= url) tier
                newAnnounceList = map (\x -> if x == tier then newTier else x) announceList
            modify $ \s -> s { _announceList = newAnnounceList }
            return ()


buildRequest :: StatusState -> B.ByteString -> Process PConf PState String
buildRequest torrentStatus url = do
    params <- urlEncodeVars `fmap` buildRequestParams torrentStatus
    let announce = B8.unpack url
        separator = if '?' `elem` announce then "&" else "?"
    return $ concat [announce, separator, params]


buildRequestParams :: StatusState -> Process PConf PState [(B.ByteString, B.ByteString)]
buildRequestParams torrentStatus = do
    infohash  <- asks _infoHash
    peerId    <- gets _peerId
    localPort <- gets _localPort
    status    <- gets _trackerStatus
    return $
        [ (B8.pack "port", B8.pack $ show localPort)
        , (B8.pack "peer_id", B8.pack $ peerId)
        , (B8.pack "info_hash", infohash)
        , (B8.pack "left", B8.pack . show $ _sLeft torrentStatus)
        , (B8.pack "uploaded", B8.pack . show $ _sUploaded torrentStatus)
        , (B8.pack "downloaded", B8.pack . show $ _sDownloaded torrentStatus)
        , (B8.pack "compact", B8.pack "1")
        ] ++ (event status)
  where
    event e = case e of
        Running   -> []
        Stopped   -> [(B8.pack "event", B8.pack "stopped")]
        Started   -> [(B8.pack "event", B8.pack "started")]
        Completed -> [(B8.pack "event", B8.pack "completed")]


processResult :: BCode -> TrackerResponse
processResult bc =
    case BCode.trackerError bc of
        Just e -> ResponseError e
        Nothing -> case BCode.trackerWarning bc of
            Just w -> ResponseWarning w
            Nothing -> case decode bc of
                Just ok -> ok
                Nothing -> ResponseDecodeError $ B8.pack "Не удалось распознать ответ от сервера"
  where
    decode bc = ResponseNormal
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
        port = S.PortNum (cw16 portCoded)
        in (Peer $ S.SockAddrInet port ip) : decodeIp4 remain
    | otherwise = []

decodeIp6 :: B.ByteString -> [Peer]
decodeIp6 bs
    | B.null bs = []
    | B.length bs >= 18 = let
        (peer, remain) = B.splitAt 18 bs
        (ipCoded, portCoded) = B.splitAt 16 peer
        ip = cW128 ipCoded
        port = S.PortNum (cw16 portCoded)
        in (Peer $ S.SockAddrInet6 port 0 ip 0) : decodeIp6 remain
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


