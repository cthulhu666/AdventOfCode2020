import Data.List (sort)

diffs :: [Int] -> [Int]
diffs [] = []
diffs [x] = [3]
diffs (x:y:xs) = (y-x) : diffs (y:xs)

solve :: [Int] -> Int
solve xs = x * y
    where
     x = length $ filter(==1) ys
     y = length $ filter(==3) ys
     ys = diffs $ 0 : sort xs

main = do
    str <- getContents
    print $ solve [read x | x <- lines str]
