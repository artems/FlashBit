import Control.Concurrent
import Control.Concurrent.STM
import System.Random

import Control.Monad
import Control.Monad.State

import Process
import Server
import Supervisor


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
    let (time, gen') = randomR(1000 * 100, 3000 * 100) gen
    setStdGen gen'
    return time


data MyState = MyState
    { name  :: String
    , role  :: String
    , skill :: String
    , chan  :: TChan MyMessage
    }

data MyMessage = MyTerminate


runMusican role skill chan = do
    name <- pickName
    time <- pickTime
    let state = MyState name role skill chan
    runServer () state onInit (server time)
  where
    server time
        = mkServerWithTimeout wait onMessage terminate time onTimeout


onInit :: Process () MyState (Maybe Reason)
onInit = do
    n <- gets name
    r <- gets role
    liftIO . putStrLn $ "Musician " ++ n ++ ", playing the " ++ r ++ " entered the room"
    return Nothing


wait :: Process () MyState MyMessage
wait = do
    c <- gets chan
    liftIO . atomically $ readTChan c


onMessage :: MyMessage -> Process () MyState (Maybe Reason)
onMessage MyTerminate = return $ Just Shutdown


onTimeout :: Process () MyState (Maybe Reason)
onTimeout = do
    n <- gets name
    s <- gets skill

    if s == "good"
        then do
            liftIO $ putStrLn $ n ++ " produced sound!"
            return Nothing
        else do
            dice <- liftIO $ randomRIO ((1, 5) :: (Int, Int))
            case dice of
                1 -> do
                    liftIO $ putStrLn $ n ++ " played a false note. Uh oh"
                    return $ Just $ UserReason "bad note"
                _ -> do
                    liftIO $ putStrLn $ n ++ " produced sound!"
                    return Nothing


terminate :: Reason -> Process () MyState ()
terminate reason = do
    n <- gets name
    r <- gets role
    case reason of
        Normal ->
            liftIO $ putStrLn $ n ++ " left the room (" ++ r ++")"
        Shutdown ->
            liftIO $ putStrLn $ "The manager is mad and fired the whole band! " ++
               n ++ " just got back to playing in the subway"
        _ ->
            liftIO $ putStrLn $ n ++ " has been kicked out (" ++ r ++ ")"


musicanWorker role skill = do
    chan <- newTChanIO
    return $ ChildSpec
        { csType = Worker
        , csAction = runMusican role skill chan
        , csRestart = Permanent
        , csShutdown = do
            putStrLn "user shutdown"
            atomically $ writeTChan chan MyTerminate
        , csShutdownTimeout = 1000
        }

startSupervisor = do
    stop <- newEmptyTMVarIO
    let specs = do
        spec1 <- musicanWorker "singer" "good"
        spec2 <- musicanWorker "bass" "good"
        spec3 <- musicanWorker "drum" "bad"
        spec4 <- musicanWorker "keytar" "good"
        -- return $ [("singer", spec1), ("bass", spec2), ("drum", spec3), ("keytar", spec4)]
        return $ [("singer", spec1), ("drum", spec3)]
    runSupervisor OneForAll 3 60 specs


main = startSupervisor

