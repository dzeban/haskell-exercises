import Data.List
import Data.Ord

count :: [a] -> Int
count (x:xs) = 1 + (count xs)
count [] = 0

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (count xs)

makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength xss = sortBy (\xs ys -> compare (length xs) (length ys)) xss

intersperse' :: a -> [[a]] -> [a]
intersperse' e [] = []
intersperse' e [x] = x
intersperse' e (x:xs) = x ++ [e] ++ intersperse' e xs

data Tree a = Empty | Node a (Tree a) (Tree a)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)
