import Data.List (nub)
import Data.Strings (strNull)

parse [] = []
parse xs = x : parse (drop (length x + 1) xs)
    where x = takeWhile (not . strNull) xs

solve xs = sum ys
    where
        ys = [uniq x | x <- xs]
        uniq = length . nub . foldr (++) []

main = do
    str <- readFile "input.txt"
    print $ solve . parse . lines $ str
