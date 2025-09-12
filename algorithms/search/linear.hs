linearSearch :: Eq a => a -> [a] -> Maybe Int
linearSearch target xs = lookup target (zip xs [0..])

main = print $ linearSearch 5 [3,1,4,1,5,9,2,6,5]