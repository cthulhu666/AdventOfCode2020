import Data.Strings

data Row = Row Int Int Char String
            deriving (Show)

parse str = Row (read x :: Int) (read y :: Int) (ch!!0) s2
    where
        (x, y) = strSplit "-" r
        [r, ch] = words s1
        (s1, s2) = strSplit ": " str

solve :: [Row] -> Int
solve rows = length xs
    where xs = [x | x <- rows, valid x]

valid :: Row -> Bool
valid (Row x y ch str) = (str!!(x-1) == ch) /= (str!!(y-1) == ch)

main = do
    str <- readFile "input.txt"
    print $ solve (map parse ([x | x <- lines str]))
