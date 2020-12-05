import Data.Set (fromList, isSubsetOf)
import Data.Strings (strNull, strSplit)

requiredFields = fromList ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

solve xs = length $ filter id (map valid xs)

valid :: [(String, String)] -> Bool
valid xs = isSubsetOf requiredFields keys
    where keys = fromList $ map fst xs

parseAll :: [String] -> [[(String, String)]]
parseAll [] = []
parseAll xs = parse x : parseAll (drop (length x + 1) xs)
    where x = takeWhile (not . strNull) xs

parse xs = [strSplit ":" x | x <- ys]
    where ys = [y | x <- xs, y <- words x]

main = do
    str <- readFile "input.txt"
    print $ solve . parseAll . lines $ str
