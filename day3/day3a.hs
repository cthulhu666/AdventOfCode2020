solve xs = length $ filter id (traverse' 3 (tail xs))

traverse' i [] = []
traverse' i (x:xs) = detect x i : traverse' (i+3) xs
    where
        detect line pos = (cycle line)!!pos == '#'

main = do
    str <- readFile "input.txt"
    print $ solve [x | x <- lines str]
