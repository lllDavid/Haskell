import Control.Concurrent (threadDelay)
import Data.Set (Set)
import qualified Data.Set as Set

-- Linear Congruential Generator
lcg :: Integer -> [Double]
lcg seed = map (\x -> fromIntegral (x `mod` 10000) / 10000.0)
                 $ iterate (\x -> (1103515245 * x + 12345) `mod` 2147483648) seed

-- Log types
data LogType = Info | Warning | Error deriving (Show, Eq)

-- Simulated source IPs
type IP = String
ips :: [IP]
ips = ["192.168.0.1", "10.0.0.5", "172.16.0.3", "203.0.113.7"]

-- Convert LCG value to LogType
logFromValue :: Double -> LogType
logFromValue v
    | v < 0.1   = Error    -- 10%
    | v < 0.2   = Warning  -- 10%
    | otherwise = Info     -- 80%

-- Convert LCG value to IP
ipFromValue :: Double -> IP
ipFromValue v = ips !! floor (v * fromIntegral (length ips))

-- Infinite log stream using LCG
logStream :: Integer -> [(IP, LogType)]
logStream seed = zip ipsStream logTypesStream
  where
    values = lcg seed
    ipsStream = map ipFromValue values
    logTypesStream = map logFromValue values

-- Process logs with persistent blocked set
processLogs :: [(IP, LogType)] -> Set IP -> IO ()
processLogs [] _ = return ()
processLogs ((ip, logType):rest) blocked = do
    let blocked' = if logType == Error then Set.insert ip blocked else blocked
    if ip `Set.member` blocked
        then putStrLn $ ip ++ " is blocked. Log ignored."
        else putStrLn $ ip ++ " " ++ show logType
    threadDelay 300000  -- simulate delay
    processLogs rest blocked'

main :: IO ()
main = do
    putStrLn "Starting simulated Intrusion Prevention (persistent blocking)..."
    let logs = logStream 42
    processLogs logs Set.empty
