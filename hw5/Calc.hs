module Calc where
import ExprT
import Parser(parseExp)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
    Just expr -> Just (eval expr)
    Nothing -> Nothing
