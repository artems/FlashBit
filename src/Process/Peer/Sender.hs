module Process.Peer.Sender
    ( start
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader (liftIO, asks)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Network.Socket (Socket)
import qualified Network.Socket.ByteString as SB

import Process
import Process.Peer.Chan
import Protocol.Peer
import Server hiding (start)
import qualified Server


data PState = PState
    { sSocket :: Socket
    , sHandshake :: Handshake
    }


start :: Socket -> Handshake -> TChan Message -> (Reason -> IO ()) -> IO (TMVar Reason)
start sock handshake chan userTermination = do
    let state = PState sock handshake
    Server.start state chan server userTermination


-- TODO timeout => send keep-alive
server :: Server PState Message
server = dummyServer
    { srvInit = sendHandshake
    , srvOnMessage = sendMessage
    }


sendHandshake :: PState -> IO (Response PState)
sendHandshake state@(PState { sSocket = sock, sHandshake = handshake }) = do
    liftIO . putStrLn $ "send handshake"
    SB.sendAll sock (encodeHandshake handshake)
    return $ Right state


sendMessage :: PState -> Message -> IO (Response PState)
sendMessage state@(PState { sSocket = sock }) message = do
    SB.sendAll sock (encodeMessage message)
    return $ Right state


