import Data.List

solve :: [Int] -> Int
solve xs = x*y
    where
        (x,y) = head [ (x,y) | (x:rest) <- tails xs , y <- rest , x+y==2020 ]

main = do
    str <- readFile "input.txt"
    print $ solve [read x :: Int | x <- lines str]
