module Stream
where

-- | Unbound list
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a:(streamToList s)

instance Show a => Show (Stream a) where
    show s = take 20 (show (streamToList s))

streamRepeat :: a -> Stream a
streamRepeat c = Cons c (streamRepeat c)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

