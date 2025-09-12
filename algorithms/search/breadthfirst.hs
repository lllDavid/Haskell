bfs :: Eq a => Graph a -> a -> [a]
bfs graph start = go [] [start]
  where
    go visited [] = visited
    go visited (x:xs)
      | x `elem` visited = go visited xs
      | otherwise        = go (visited ++ [x]) (xs ++ neighbors)
      where neighbors = maybe [] id (lookup x graph)

main = print $ bfs graphExample 'A'