qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch target xs = go 0 (length xs - 1)
  where
    go low high
      | low > high = Nothing
      | xs !! mid < target = go (mid + 1) high
      | xs !! mid > target = go low (mid - 1)
      | otherwise = Just mid
      where mid = (low + high) `div` 2

main = print $ binarySearch 5 (qsort [3,1,4,1,5,9,2,6,5])