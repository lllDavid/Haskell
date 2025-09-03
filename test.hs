lcg :: Integer -> [Double]
lcg seed = map (\x -> fromIntegral (x `mod` 10000) / 10000.0) $ iterate (\x -> (1103515245 * x + 12345) `mod` 2147483648) seed

s0, k, r, sigma, t :: Double
s0 = 100
k = 110
r = 0.05
sigma = 0.2
t = 1.0

nSim :: Int
nSim = 10000

simulatePrice :: Double -> Double -> Double -> Double -> Double -> Double
simulatePrice s0 r sigma t z = s0 * exp ((r - 0.5 * sigma^2) * t + sigma * sqrt t * z)

monteCarloPrice :: Int -> Double
monteCarloPrice n =
    let zs = take n (lcg 42) 
        prices = map (simulatePrice s0 r sigma t) zs
        payoffs = map (\st -> max (st - k) 0) prices
        avgPayoff = sum payoffs / fromIntegral n
    in exp (-r * t) * avgPayoff

main :: IO ()
main = do
    let price = monteCarloPrice nSim
    putStrLn $ "European Call Option Price: " ++ show price