{-# LANGUAGE MultiParamTypeClasses  #-}

import Control.Concurrent
import Control.Concurrent.STM
import System.Random

import Server hiding (start)
import qualified Server
import Supervisor hiding (start)
import qualified Supervisor


firstname = ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
    "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"]
lastname = ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
    "Terese", "Tennelli", "Jamal", "Li", "Perlstein"]

pickName :: IO String
pickName = do
    gen <- getStdGen
    let (firstIdx, gen') = randomR (0, 9) gen
        (lastIdx, gen'') = randomR (0, 9) gen'
    setStdGen gen''
    return $ (firstname !! firstIdx) ++ " " ++ (lastname !! lastIdx)

pickTime :: IO Int
pickTime = do
    gen <- getStdGen
    let (time, gen') = randomR(2000 * 1000, 5000 * 1000) gen
    setStdGen gen'
    return time


data MyState = MyState
    { name  :: String
    , role  :: String
    , skill :: String
    }

data MyMessage = MyTerminate


musicanServer time = simpleServer
    { srvTimeout   = Just time
    , srvInit      = onInit
    , srvOnTimeout = timeout
    , srvOnMessage = receive
    , srvTerminate = terminate
    }

onInit :: MyState -> IO (Response MyState)
onInit state@(MyState { name = n, role = r }) = do
    putStrLn $ "Musician " ++ n ++ ", playing the " ++ r ++ " entered the room"
    return $ Right state

receive :: MyState -> MyMessage -> IO (Response MyState)
receive state MyTerminate = do
    return $ Left Normal

timeout :: MyState -> IO (Response MyState)
timeout state@(MyState { name = n, skill = "good" }) = do
    putStrLn $ n ++ " produced sound!"
    return $ Right state

timeout state@(MyState { name = n, skill = "bad" }) = do
    dice <- randomRIO ((1, 5) :: (Int, Int))
    case dice of
        1 -> do
            putStrLn $ n ++ " played a false note. Uh oh"
            return $ Left $ UserReason "bad note"
        _ -> do
            putStrLn $ n ++ " produced sound!"
            return $ Right state


terminate (MyState { name = n, role = r }) Normal = do
    putStrLn $ n ++ " left the room (" ++ r ++")"
    return Normal

terminate (MyState { name = n, role = r }) Shutdown = do
    putStrLn $ "The manager is mad and fired the whole band! " ++
               n ++ " just got back to playing in the subway"
    return Normal

terminate (MyState { name = n, role = r }) (UserReason "bad note") = do
    putStrLn $ n ++ " sucks! kicked that member out of the band! (" ++ r ++ ")"
    return Normal

terminate (MyState { name = n, role = r }) reason = do
    putStrLn $ n ++ " has been kicked out (" ++ r ++ ")"
    return Normal


startServer role skill = do
    name <- pickName
    time <- pickTime
    chan <- newTChanIO
    stop <- newEmptyTMVarIO
    let state = MyState name role skill
        server = musicanServer time
    return $ Server.start state chan server

startSupervisor = do
    stop <- newEmptyTMVarIO
    mServer <- startServer "bass" "bad"
    let spec = ChildSpec
            { csType = Worker
            , csAction = mServer
            , csRestart = Permanent
            , csShutdown = 0
            }
        finally = \r -> atomically $ putTMVar stop r
    Supervisor.start OneForOne 5 60 [("singer", spec)] finally


main = do
    (stop, chan) <- startSupervisor
    reason <- atomically $ takeTMVar stop
    print reason
    return ()


