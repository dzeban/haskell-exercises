data List a = Empty | Cons a (List a)
              deriving Show

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Empty

toList :: List a -> [a]
toList (Cons x xs) = x:(toList xs)
toList Empty = []
