import Data.Set (fromList, intersection)
import Data.Strings (strNull)

parse [] = []
parse xs = x : parse (drop (length x + 1) xs)
    where x = takeWhile (not . strNull) xs

solveAll = sum . map solve
    where solve = length . foldr1 intersection . map fromList

main = do
    str <- readFile "input.txt"
    print $ solveAll . parse . lines $ str
