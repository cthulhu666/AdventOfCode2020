import Data.List

solve xs = x*y*z
    where
        (x,y,z) = head [(x,y,z) | (x,y,z) <- triples xs, x+y+z == 2020]

-- https://stackoverflow.com/questions/28191103/iterate-over-all-pair-combinations-without-repetition-in-haskell
pairs xs = [ (x,y) | (x:rest) <- tails xs , y <- rest ]

triples [] = []
triples (x:xs) = map (\(y,z) -> (x,y,z)) (pairs xs) ++ triples xs

main = do
    str <- readFile "input.txt"
    print $ solve [read x :: Int | x <- lines str]
