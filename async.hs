import Control.Concurrent
import Control.Monad (forM_)

task :: String -> Int -> IO ()
task name delayTime = do
    threadDelay (delayTime * 1000000)
    putStrLn $ name ++ " done!"

main :: IO ()
main = do
    mvars <- mapM (\_ -> newEmptyMVar) [1..2]

    forkIO $ task "Task 1" 2 >> putMVar (mvars !! 0) ()
    forkIO $ task "Task 2" 3 >> putMVar (mvars !! 1) ()

    forM_ mvars takeMVar

    putStrLn "All tasks finished."