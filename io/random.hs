import System.Random

main = do
    g <- getStdGen

    -- Supply concrete types for r1 and r2,
    -- because  1 and 10 could be Int, Integer, Integral
    -- and it's confusing randomR
    let r1 = 1 :: Int
    let r2 = 10 :: Int

    print $ fst $ randomR (r1, r2) g
