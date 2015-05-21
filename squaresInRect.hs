module Codewars.Kata.Rectangle where

squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect lng wdth
	| lng == wdth = Nothing
	| lng  == 1   = 1
	| wdth == 1   = 1
	| lng > wdth  = squaresInRect (lng - wdth) wdth : []
	| otherwise   = squaresInRect lng (wdth - lng) : []
