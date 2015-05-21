module Banjo where

areYouPlayingBanjo :: String -> String
areYouPlayingBanjo name@('R':xs) = name ++ " plays banjo"
areYouPlayingBanjo name@('r':xs) = name ++ " plays banjo"
areYouPlayingBanjo name = name ++ " does not play banjo"
