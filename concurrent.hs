import Control.Concurrent
import Control.Monad
import Data.List (sortOn)

worker :: Int -> MVar (Int, Integer) -> IO ()
worker n resultVar = do
    let result = product [1..fromIntegral n]
    putStrLn $ "Computed factorial of " ++ show n ++ ": " ++ show result
    putMVar resultVar (n, result)

runTasks :: Int -> [Int] -> IO [(Int, Integer)]
runTasks maxThreads tasks = do
    sem <- newQSem maxThreads      
    resultVars <- replicateM (length tasks) newEmptyMVar

    forM_ (zip tasks resultVars) $ \(n, var) -> forkIO $ do
        waitQSem sem             
        worker n var             
        signalQSem sem          

    results <- mapM takeMVar resultVars
    return $ sortOn fst results   

main :: IO ()
main = do
    let numbers = [1..20]         
        maxThreads = 4            

    results <- runTasks maxThreads numbers
    putStrLn $ "All results: " ++ show results