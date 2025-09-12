ssort :: (Ord a) => [a] -> [a]
ssort [] = []
ssort xs = let x = minimum xs in x : ssort (remove x xs)
  where
    remove _ [] = []
    remove y (z:zs)
      | y == z    = zs
      | otherwise = z : remove y zs

main = print $ ssort [3,1,4,1,5,9,2,6,5]