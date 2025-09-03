{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Maybe (fromJust)

portfolioReturns :: [Double]
portfolioReturns = [-0.02, 0.01, -0.005, 0.015, -0.01, 0.02, -0.03, 0.005]

valueAtRisk :: Double -> [Double] -> Double
valueAtRisk confidence returns =
    let sortedReturns = sort returns
        index = floor $ (1 - confidence) * fromIntegral (length returns)
    in sortedReturns !! index

main :: IO ()
main = do
    let confidence = 0.95
        var = valueAtRisk confidence portfolioReturns
    putStrLn $ "Portfolio VaR at " ++ show (confidence*100) ++ "% confidence: " ++ show (abs var)