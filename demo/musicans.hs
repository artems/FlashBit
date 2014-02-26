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
    let (time, gen') = randomR(2000 * 100, 5000 * 100) gen
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
    return Shutdown

terminate (MyState { name = n, role = r }) reason@(UserReason "bad note") = do
    putStrLn $ n ++ " sucks! kicked that member out of the band! (" ++ r ++ ")"
    return reason

terminate (MyState { name = n, role = r }) reason = do
    putStrLn $ n ++ " has been kicked out (" ++ r ++ ")"
    return reason


startServer role skill finally = do
    name <- pickName
    time <- pickTime
    chan <- newTChanIO
    stop <- newEmptyTMVarIO
    let state = MyState name role skill
        server = musicanServer time
    Server.start state chan server finally

startSupervisor = do
    stop <- newEmptyTMVarIO
    let spec1 = ChildSpec
            { csType = Worker
            , csAction = \f -> startServer "singer" "good" f
            , csRestart = Permanent
            , csShutdown = 1000
            }
        spec2 = ChildSpec
            { csType = Worker
            , csAction = \f -> startServer "bass" "good" f
            , csRestart = Temporary
            , csShutdown = 1000
            }
        spec3 = ChildSpec
            { csType = Worker
            , csAction = \f -> startServer "drum" "bad" f
            , csRestart = Transient
            , csShutdown = 1000
            }
        spec4 = ChildSpec
            { csType = Worker
            , csAction = \f -> startServer "keytar" "good" f
            , csRestart = Transient
            , csShutdown = 1000
            }
        specs = [("singer", spec1), ("bass", spec2), ("drum", spec3), ("keytar", spec4)]

        finally reason = atomically $ putTMVar stop reason
    Supervisor.start OneForAll 3 60 specs finally
    return stop


main = do
    stop <- startSupervisor
    reason <- atomically $ takeTMVar stop
    print reason
    return ()


