data Slope = Slope Int Int
    deriving Show

slopes = [Slope 1 1,
          Slope 3 1,
          Slope 5 1,
          Slope 7 1,
          Slope 1 2]

solveAll xs slopes = product $ map (\s -> solve xs s) slopes

solve xs slope = length $ filter id (traverse' slope 0 xs)

traverse' :: Slope -> Int -> [String] -> [Bool]
traverse' _ _ [] = []
traverse' slope i (x:xs) = (detect x i) : traverse' slope (i+h) (drop (v-1) xs)
    where
        (Slope h v) = slope
        detect line pos = (cycle line)!!pos == '#'

main = do
    str <- readFile "input.txt"
    print $ solveAll [x | x <- lines str] slopes
