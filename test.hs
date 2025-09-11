module Main where
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Control.Monad (forM_)

{-
// stdin
main :: IO ()
main = do
    input <- getLine
    putStrLn input
-}

{-
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
-}

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]

primesUpTo :: Int -> [Int]
primesUpTo n = filter isPrime [2..n]

filterPrimes :: Maybe Int -> [Int] -> [Int]
filterPrimes Nothing xs  = xs
filterPrimes (Just m) xs = filter (>= m) xs

main :: IO ()
main = do
    args <- getArgs
    case args of
        (nArg:rest) -> case readMaybe nArg of
            Just n -> do
                let minFilter = case rest of
                                  (mArg:_) -> readMaybe mArg
                                  []       -> Nothing
                let primes = filterPrimes minFilter (primesUpTo n)
                putStrLn $ "Primes: " ++ show primes
            Nothing -> putStrLn "First argument must be an integer"
        [] -> putStrLn "Usage: program <max> [minFilter]"