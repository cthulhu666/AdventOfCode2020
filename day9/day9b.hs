import Data.List (inits)

preambleLength = 25

solve :: [Int] -> Maybe Int
solve xs = fmap (\x -> minimum x + maximum x) (find x xs)
    where Just x = findInvalid xs

find :: Int -> [Int] -> Maybe [Int]
find _ [] = Nothing
find n xs =
    case l of
        Nothing -> find n $ tail xs
        Just x -> Just x
    where l = lookup n $ zip (map sum $ inits xs) (inits xs)

findInvalid xs = lookup False $ zip bs (drop preambleLength xs)
    where bs = check (take preambleLength xs) (drop preambleLength xs)

check preamble [] = []
check preamble (x:xs) = elem x (sums . pairs $ preamble) : check (tail preamble ++ [x]) xs

pairs xs = [(x, y) | x <- xs, y <- xs, x/=y]

sums xs = [x+y | (x,y) <- xs]

main = do
    str <- getContents
    print $ solve . map (read) $ lines str
