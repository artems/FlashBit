module Main () where

import Data.Word
import Data.Bits ((.&.), shiftL, shiftR)
import Data.Char (digitToInt)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Control.Concurrent.STM
import Network.Socket (Socket)
import qualified Network.Socket as S

import Protocol.Peer
import qualified Process.Peer.Supervisor


peerId = "-AT00-123asd213dfrae"

infohash = B.pack
    [106, 54, 222, 32, 29, 242, 241, 178, 200, 23, 71, 76, 48, 117, 255, 14, 170, 140, 119, 133]

handshake = Handshake peerId infohash []


run addr = do
    let [host, port] = splitWhen (== ':') addr
        port1 = (read port :: Word16)
        port2 = ((port1 .&. 0x00FF) `shiftL` 8) + ((port1 .&. 0xFF00) `shiftR` 8)
        host1 = convert host
        sock  = S.SockAddrInet (S.PortNum port2) host1
    Process.Peer.Supervisor.runPeer sock handshake


convert :: (Integral a) => String -> a
convert host = fromIntegral . collect . map toInt . splitWhen (== '.') $ host

collect :: [Int] -> Int
collect [a4, a3, a2, a1] =
    shiftL a1 24 + shiftL a2 16 + shiftL a3 8 + a4

toInt :: String -> Int
toInt s = foldl (\acc c -> acc * 10 + digitToInt c) 0 s

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s
    = case dropWhile p s of
        "" -> []
        s' -> w : splitWhen p s''
            where (w, s'') = break p s'


