type Graph a = [(a, [a])]

dfs :: Eq a => Graph a -> a -> [a]
dfs graph start = go [] [start]
  where
    go visited [] = visited
    go visited (x:xs)
      | x `elem` visited = go visited xs
      | otherwise        = go (visited ++ [x]) (neighbors ++ xs)
      where neighbors = maybe [] id (lookup x graph)

graphExample :: Graph Char
graphExample = [('A',['B','C']), ('B',['D']), ('C',['E']), ('D',[]), ('E',[])]

main = print $ dfs graphExample 'A' 