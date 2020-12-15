import Data.Strings (strSplitAll)
import Math.NumberTheory.Moduli.Chinese

-- ad hoc solution for test input
-- head $ [x | x <- [1..], x `rem` 7 == 0, x `rem` 13 == (13-1), x `rem` 59 == (59-4), x `rem` 31 == (31-6), x `rem` 19 == (19-7)]
-- or
-- ys1 = [x | x <- [1..], x `rem` 7 == 0]
-- ys2 = [x | x <- ys1, (x + 1) `rem` 13 == 0]
-- ys3 = [x | x <- ys2, (x + 4) `rem` 59 == 0]
-- ys4 = [x | x <- ys3, (x + 6) `rem` 31 == 0]
-- ys5 = [x | x <- ys4, (x + 7) `rem` 19 == 0]
-- head ys5

parse :: String -> [(Integer,Integer)]
parse str = ys
    where
        ls = lines str
        xs = strSplitAll "," $ ls !! 1
        ys = map (\(x,y) -> (x, read y)) $ filter (\(_,y) -> y /= "x") $ zip [0,1..] xs

-- after spending whole day trying to figure it out
-- I went to reddit and learned about Chinese Remainder Theorem
solve :: [(Integer,Integer)] -> Maybe Integer
solve xs = chineseRemainder $ map (\(n,m) -> ((m-n) `mod` m, m)) xs


main = do
    str <- getContents
    print $ solve $ parse str
