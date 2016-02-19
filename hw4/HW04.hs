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

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 1 Leaf a Leaf
insert a (Node i left n right) =
    if height left <= height right then
        Node (height newLeft + 1) newLeft n right
    else
        Node (height newRight + 1) left n newRight
    where
        newLeft = insert a left
        newRight = insert a right

height :: Tree a -> Integer
height Leaf = 0
height (Node i _ _ _) = i

xor' :: [Bool] -> Bool
xor' xs = odd . length $ filter (==True) xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

l = [3..10]
