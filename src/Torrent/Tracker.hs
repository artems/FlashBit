module Torrent.Tracker
    ( trackerPeers
    , trackerError
    , trackerWarning
    , trackerInterval
    , trackerMinInterval
    , trackerComplete
    , trackerIncomplete
    ) where

import Data.Word
import Data.Bits (Bits, shiftL)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Network.Socket as S

import Torrent.BCode


trackerPeers :: BCode -> Maybe [S.SockAddr]
trackerPeers bc = do
    v4 <- return $ fromMaybe (B.empty) (searchStr "peers" bc)
    v6 <- return $ fromMaybe (B.empty) (searchStr "peers6" bc)
    return $ decodeIp (v4, v6)

trackerError :: BCode -> Maybe ByteString
trackerError = searchStr "failure reason"

trackerWarning :: BCode -> Maybe ByteString
trackerWarning = searchStr "warning message"

trackerInterval :: BCode -> Maybe Integer
trackerInterval = searchInt "interval"

trackerMinInterval :: BCode -> Maybe Integer
trackerMinInterval = searchInt "min interval"

trackerComplete :: BCode -> Maybe Integer
trackerComplete = searchInt "complete"

trackerIncomplete :: BCode -> Maybe Integer
trackerIncomplete = searchInt "incomplete"

decodeIp :: (ByteString, ByteString) -> [S.SockAddr]
decodeIp (ip4, ip6) = decodeIp4 ip4 ++ decodeIp6 ip6

decodeIp4 :: ByteString -> [S.SockAddr]
decodeIp4 bs
    | B.null bs = []
    | B.length bs >= 6 =
        let (peer, remain) = B.splitAt 6 bs
            (ipCoded, portCoded) = B.splitAt 4 peer
            ip = cW ipCoded
            port = S.PortNum (cW portCoded)
        in (S.SockAddrInet port ip) : decodeIp4 remain
    | otherwise = []

decodeIp6 :: ByteString -> [S.SockAddr]
decodeIp6 bs
    | B.null bs = []
    | B.length bs >= 18 = let
        (peer, remain) = B.splitAt 18 bs
        (ipCoded, portCoded) = B.splitAt 16 peer
        ip = cW128 ipCoded
        port = S.PortNum (cW portCoded)
        in (S.SockAddrInet6 port 0 ip 0) : decodeIp6 remain
    | otherwise = []

cW :: (Integral a, Bits a) => ByteString -> a
cW bs = foldr (\w8 acc -> acc `shiftL` 8 + fromIntegral w8) 0 (B.unpack bs)

cW128 :: ByteString -> (Word32, Word32, Word32, Word32)
cW128 bs = let
    (q1, r1) = B.splitAt 4 bs
    (q2, r2) = B.splitAt 4 r1
    (q3, q4) = B.splitAt 4 r2
    in (cW q1, cW q2, cW q3, cW q4)
