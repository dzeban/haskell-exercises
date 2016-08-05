data Fruit = Apple | Orange
             deriving Show

apple = "apple"
orange = "orange"

-- This doesn't work, because apple and orange here are locally binded variables
-- whichFruit :: String -> Fruit
-- whichFruit f = case f of
--                  apple -> Apple
--                  orange -> Orange

whichFruit :: String -> Fruit
whichFruit f
    | f == apple = Apple
    | f == orange = Orange
