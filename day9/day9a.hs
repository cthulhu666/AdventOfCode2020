preambleLength = 25

solve xs = lookup False $ zip bs (drop preambleLength xs)
    where bs = check (take preambleLength xs) (drop preambleLength xs)

check preamble [] = []
check preamble (x:xs) = elem x (sums . pairs $ preamble) : check (tail preamble ++ [x]) xs

pairs xs = [(x, y) | x <- xs, y <- xs, x/=y]

sums xs = [x+y | (x,y) <- xs]

main = do
    str <- getContents
    print $ solve . map (read) $ lines str
