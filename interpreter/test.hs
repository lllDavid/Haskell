{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.String (Parser)

data Expr
    = Val Integer
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving Show

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = spaces *> (parens expr <|> number) <* spaces

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* spaces <* char ')'

number :: Parser Expr
number = Val . read <$> many1 digit

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (char '+' *> pure Add) <|> (char '-' *> pure Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = (char '*' *> pure Mul) <|> (char '/' *> pure Div)

eval :: Expr -> Either String Integer
eval (Val x)   = Right x
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = do
    denominator <- eval b
    if denominator == 0
        then Left "Division by zero"
        else div <$> eval a <*> pure denominator

repl :: IO ()
repl = do
    putStrLn "Tiny Haskell Arithmetic Interpreter (type :q to quit)"
    loop
  where
    loop = do
        putStr "> "
        line <- getLine
        if line == ":q" then putStrLn "Bye!" else do
            case parse expr "" line of
                Left err -> print err
                Right ast -> print $ eval ast
            loop

main :: IO ()
main = repl