module Golf where

-----------
-- Skips --
-----------

every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
    (y:ys) -> y : every n ys
    []     -> []

skips :: [a] -> [[a]]
skips xs = map (\n -> every n xs) [1..length xs]

------------------
-- Local Maxima --
------------------

take3 n xs = take 3 (drop n xs)

triads xs = map (\n -> take3 n xs) [0..length xs - 3]

findLocalMaxima :: [Integer] -> [Integer]
findLocalMaxima (x1:x2:x3:[]) = if x2 > x1 && x2 > x3 then [x2] else []
findLocalMaxima [] = []

localMaxima :: [Integer] -> [Integer]
localMaxima xs = concat $ map findLocalMaxima (triads xs)


localMaxima' :: [Int] -> [Int]
localMaxima' (x:y:z:rest)
    | y > x && y > z = y : localMaxima' (y:z:rest)
    | otherwise = localMaxima' (y:z:rest)
localMaxima' _ = []


-----------------
--  Histogram --
-----------------

select :: Int -> [Int] -> [Int]
select n xs = filter (\x -> x == n) xs

selectCount :: Int -> [Int] -> Int
selectCount n xs = length $ select n xs

hist :: [Int] -> [Int]
hist xs = map (\n -> selectCount n xs) [0..9]

writeHist :: [Int] -> [String]
writeHist xs = case zeroes xs of
    True  -> []
    False -> (writeLine xs ++ "\n") : (writeHist (map decrementToZero xs))

decrementToZero :: Int -> Int
decrementToZero 0 = 0
decrementToZero x = x - 1

zeroes :: [Int] -> Bool
zeroes [0]    = True
zeroes (0:xs) = zeroes xs
zeroes (_:xs) = False

writeLine :: [Int] -> String
writeLine (x:xs) 
    | x > 0 = "*" ++ writeLine xs
    | otherwise = " " ++ writeLine xs
writeLine [] = []

histogram :: [Int] -> String
histogram xs = (concat . reverse . writeHist . hist $ xs) 
    ++ "==========\n" 
    ++ "0123456789\n"

l :: [Int]
l = [1,4,5,4,6,6,3,4,2,4,9]
