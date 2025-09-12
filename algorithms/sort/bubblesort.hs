bsort :: (Ord a) => [a] -> [a]
bsort xs = case bubble xs of
             (ys, False) -> ys
             (ys, True)  -> bsort ys
  where
    bubble (x:y:zs)
      | x > y     = let (rest, swapped) = bubble (x:zs) in (y:rest, True)
      | otherwise = let (rest, swapped) = bubble (y:zs) in (x:rest, swapped)
    bubble xs = (xs, False)

main = print $ bsort [3,1,4,1,5,9,2,6,5]