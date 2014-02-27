
import Data.Word
import Data.Bits (shiftL)
import Data.Char (digitToInt)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Control.Concurrent.STM
import Network.Socket (Socket)
import qualified Network.Socket as S

import Protocol.Peer
import Process.Peer.Chan
import qualified Process.Peer.Sender as Sender
import qualified Process.Peer.Handler as Handler
import qualified Process.Peer.Receiver as Receiver

import qualified Process.Peer.Supervisor


peerId = "-AT00-123asd213dfrae"

infohash = B.pack
    [106, 54, 222, 32, 29, 242, 241, 178, 200, 23, 71, 76, 48, 117, 255, 14, 170, 140, 119, 133]

handshake = Handshake peerId infohash []

run host port = do
    let addr = S.SockAddrInet port (convert host)
    print addr
    Process.Peer.Supervisor.start addr handshake (\reason -> print reason)


convert host = let
    a = splitWhen (== '.') host
    b = map toInt a
    c = collect b
    in fromIntegral c


collect [a4, a3, a2, a1] =
    shiftL a1 24 + shiftL a2 16 + shiftL a3 8 + a4


toInt :: String -> Int
toInt s = foldl (\acc c -> acc * 10 + digitToInt c) 0 s

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : splitWhen p s''
        where (w, s'') = break p s'



