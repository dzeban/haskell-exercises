module HW04 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

xor' :: [Bool] -> Bool
xor' xs = odd . length $ filter (==True) xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

l = [3..10]
