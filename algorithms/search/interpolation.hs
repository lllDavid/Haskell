qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

interpolationSearch :: [Int] -> Int -> Maybe Int
interpolationSearch xs target = go 0 (length xs - 1)
  where
    go low high
      | low > high || xs !! low > target || xs !! high < target = Nothing
      | xs !! low == xs !! high = if xs !! low == target then Just low else Nothing
      | otherwise =
          let pos = low + ((target - xs !! low) * (high - low)) `div` (xs !! high - xs !! low)
          in if xs !! pos == target
             then Just pos
             else if xs !! pos < target
                  then go (pos+1) high
                  else go low (pos-1)

main = print $ interpolationSearch (qsort [3,1,4,1,5,9,2,6,5]) 5 