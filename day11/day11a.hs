import Data.Matrix

buildMatrix xs = fromList w h ys
    where
        w = length xs
        h = length $ head xs
        ys = foldl1 (++) xs

run :: Matrix Char -> Matrix Char
run m = mapPos (updatePos m) m

updatePos :: Matrix Char -> (Int, Int) -> Char -> Char
updatePos m (r,c) a
    | a == 'L' && n == 0    = '#'
    | a == '#' && n >= 5    = 'L'
    | otherwise             = a
    where
        n = countOccupied $ proximity m (r,c)

solve :: Matrix Char -> Int
solve m
    | m == n    = countOccupied m
    | otherwise = solve n
    where
        n = run m

count e m = length $ filter (==e) $ toList $ m
countOccupied = count '#'

proximity m (r,c) = submatrix r1 r2 c1 c2 m
    where
        r1 = maximum [1, r-1]
        r2 = minimum [nrows m, r+1]
        c1 = maximum [1, c-1]
        c2 = minimum [ncols m, c+1]

main = do
    str <- getContents
    print $ solve $ buildMatrix $ lines $ str
