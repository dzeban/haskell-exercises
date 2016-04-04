module SundaramSieve
where

import Data.List(sort, nub)

sieveSundaram :: Int -> [Int]
sieveSundaram n = sort [x*2 + 1 | x <- doSieve [1..n]]

doSieve :: [Int] -> [Int]
doSieve xs = filter (`notElem` sieve xs) xs

sieve :: [Int] -> [Int]
sieve xs = nub [calcSieve i j | i <- xs, j <- xs, i <= j, calcSieve i j <= length xs]
    where calcSieve i j = i + j + 2*i*j
