-- dynamic programming, yay!

import Data.List (sort)

solve :: [Int] -> Int
solve xs = walk (reverse $ succs $ zs) []
    where
        ys = sort xs
        zs = 0 : ys ++ [3 + last ys]

succs :: [Int] -> [(Int, [Int])]
succs [] = []
succs (x:xs) = (x, takeWhile (<=(x+3)) xs) : succs xs


walk [x] cache = eval m cache
    where (n, m) = x
walk (x:xs) cache = (walk xs newCache)
    where
        v = eval m cache
        newCache = (n, v) : cache
        (n, m) = x

eval [] [] = 1
eval xs cache = sum ys
    where (Just ys) = sequence $ map (\x -> lookup x cache) xs

main = do
    str <- getContents
    print $ solve [read x | x <- lines str]
