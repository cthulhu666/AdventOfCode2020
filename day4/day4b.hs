import Data.Set (fromList, isSubsetOf)
import Data.Strings (strNull, strSplit)
import Text.Regex.Posix

requiredFields = fromList ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]
validations = [("byr", validByr), ("iyr", validIyr), ("eyr", validEyr),
               ("hgt", validHgt), ("hcl", validHcl), ("ecl", validEcl),
               ("pid", validPid), ("cid", \x -> True)]

-- four digits; at least 1920 and at most 2002.
validByr x = yr >= 1920 && yr <= 2002
    where yr = read x :: Int

-- four digits; at least 2010 and at most 2020.
validIyr x = yr >= 2010 && yr <= 2020
     where yr = read x :: Int

-- four digits; at least 2020 and at most 2030.
validEyr x = yr >= 2020 && yr <= 2030
     where yr = read x :: Int

-- a number followed by either cm or in:
--    If cm, the number must be at least 150 and at most 193.
--    If in, the number must be at least 59 and at most 76.
validHgt str = case suffix of
                "cm" -> v >= 150 && v <= 193
                "in" -> v >= 59 && v <= 76
                _ -> False
                where
                    suffix = drop (length str - 2) str
                    v = read $ take (length str - 2) str :: Int

-- a # followed by exactly six characters 0-9 or a-f.
validHcl :: String -> Bool
validHcl str = str =~ "^#([0-9a-f]{6})$"

-- (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
validEcl :: String -> Bool
validEcl str = str =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"

-- (Passport ID) - a nine-digit number, including leading zeroes.
validPid :: String -> Bool
validPid str = str =~ "^[0-9]{9}$"

solve xs = length $ filter id (map (\x -> valid x && valid2 x) xs)
--solve xs = length $ filter id (map valid2 xs)

valid :: [(String, String)] -> Bool
valid xs = isSubsetOf requiredFields keys
    where keys = fromList $ map fst xs

valid2 :: [(String, String)] -> Bool
valid2 xs = all id ys
    where
        ys = map validateOne xs

validateOne :: (String, String) -> Bool
validateOne (k, v) = run f v
    where
        f = lookup k validations
        run Nothing _ = False
        run (Just g) x = g x


parseAll :: [String] -> [[(String, String)]]
parseAll [] = []
parseAll xs = parse x : parseAll (drop (length x + 1) xs)
    where x = takeWhile (not . strNull) xs

parse xs = [strSplit ":" x | x <- ys]
    where ys = [y | x <- xs, y <- words x]

main = do
    str <- readFile "input.txt"
    print $ solve . parseAll . lines $ str
