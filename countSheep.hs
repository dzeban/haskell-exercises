module Codewars.Kata.Sheep where

countSheep :: [Bool] -> Int
countSheep (x:xs) = count + countSheep xs 
					where count = if x == True then 1 else 0
countSheep [] = 0
