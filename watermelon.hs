module Codewars.Kata.Watermelon where

divide :: Integer -> Bool
divide w 
	| (w > 2) && (mod w 2 == 0) = True
	| otherwise = False
