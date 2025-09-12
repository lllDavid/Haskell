isort :: (Ord a) => [a] -> [a]
isort = foldr insert []
  where
    insert x [] = [x]
    insert x ys@(y:ys')
      | x <= y    = x : ys
      | otherwise = y : insert x ys'

main = print $ isort [3,1,4,1,5,9,2,6,5]
