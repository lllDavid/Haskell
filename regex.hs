data Regex
    = Empty                  
    | Literal Char           
    | AnyChar                
    | Concat Regex Regex     
    | Star Regex             
    deriving (Show, Eq)

match :: Regex -> String -> Bool
match Empty "" = True
match Empty _  = False
match (Literal c) [x] = c == x
match (Literal _) _ = False
match AnyChar [_] = True
match AnyChar _ = False
match (Concat r1 r2) s =
    or [match r1 s1 && match r2 s2 | (s1,s2) <- splits s]
  where
    splits str = [(take n str, drop n str) | n <- [0..length str]]
match (Star r) s =
    s == "" || or [not (null s1) && match r s1 && match (Star r) s2 | (s1,s2) <- splits s]
  where
    splits str = [(take n str, drop n str) | n <- [1..length str]]

exampleRegex :: Regex
exampleRegex = Concat (Literal 'a') (Concat (Star AnyChar) (Literal 'b'))

main :: IO ()
main = do
    print $ match exampleRegex "ab"       
    print $ match exampleRegex "axxxb"    
    print $ match exampleRegex "a"        
    print $ match exampleRegex "b"        