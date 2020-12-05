import Data.Strings

data Row = Row Int Int Char String
            deriving (Show)

parse str = Row (read from :: Int) (read to :: Int) (ch!!0) y
    where
        (from, to) = strSplit "-" r
        [r, ch] = words x
        (x, y) = strSplit ": " str

solve :: [Row] -> Int
solve rows = length xs
    where xs = [x | x <- rows, valid x]

valid :: Row -> Bool
valid (Row from to ch str) = from <= cnt && cnt <= to
    where
        cnt = count ch str

count x = length . filter (x==)

main = do
    str <- readFile "input.txt"
    print $ solve (map parse ([x | x <- lines str]))
