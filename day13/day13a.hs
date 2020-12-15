import Data.List (sortOn)
import Data.Strings (strSplitAll)

parse :: String -> (Int, [Int])
parse str = (ts, ys)
    where
        ls = lines str
        ts = read $ ls!!0
        xs = strSplitAll "," $ ls !! 1
        ys = map (read) . filter (/="x") $ xs

solve :: (Int, [Int]) -> Int
solve (ts,xs) = (fst z - snd z) * fst z
    where
        ys = zip xs $ map (mod ts) xs
        z = head $ reverse $ sortOn snd ys

main = do
    str <- getContents
    print $ solve $ parse str
