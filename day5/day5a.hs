b = [1, 2, 4, 8, 16, 32, 64]

solve :: [String] -> Int
solve xs = maximum ns
    where
        ns = [ r*8+c | (r,c) <- ys ]
        ys = zip (map (row . f) xs) (map (col . g) xs)
        f = take 7
        g = drop 7

row :: String -> Int
row xs = sum [ x*y | (x,y) <- (zip ys $ reverse b) ]
    where ys = map (\x -> if x=='B' then 1 else 0) xs

col :: String -> Int
col xs = sum [ x*y | (x,y) <- (zip ys $ reverse (take 3 b)) ]
    where ys = map (\x -> if x=='R' then 1 else 0) xs

main = do
    str <- readFile "input.txt"
    print $ solve . lines $ str
