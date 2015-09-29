{-# OPTIONS_GHC -Wall #-}

-- Given an Integer return list of its digits in reverse order
-- Example: 1234 -> [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- Given an Integer return list of its digits in direct order
-- Example: 1234 -> [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Given a list of Integers, double every even starting from the end of the list
-- Example: [1,2,3,4] -> [2,2,6,4]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse . doubleEven $ reverse n

-- Given a list of Integers, double every even
-- Example: [1,2,3,4] -> [1,4,3,8]
doubleEven :: [Integer] -> [Integer]
doubleEven []         = []
doubleEven [x]        = [x]
doubleEven (x:(y:ys)) = x : (y*2) : doubleEven ys

-- Given a list of Integers, sum up every digit
-- Example: [12,34,56] = 1+2+3+4+5+6 = 21
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (sum . toDigits $ x) + (sumDigits xs)

-- Given an Integer, validate it as a correct credit card number.
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0

-- vim: expandtab: ts=4: sw=4:
