data AST
    = ASTInt Int                   
    | ASTDouble Double             
    | ASTBool Bool                 
    | ASTChar Char                 
    | ASTString String             
    | ASTList [AST]                
    | ASTTuple [AST]               
    | ASTMaybe (Maybe AST)         
    | ASTEither (Either AST AST)  
    | ASTFunc String [AST]         
    deriving (Show, Eq)

exampleTuple :: AST
exampleTuple = ASTTuple [ASTString "hello", ASTBool True, ASTDouble 3.14]

main :: IO ()
main = print exampleTuple