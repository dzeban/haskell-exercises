module Fibonacci
where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate fibOp (0, 1)

fibOp :: Num a => (a, a) -> (a, a)
fibOp (a, b) = (b, a+b)
