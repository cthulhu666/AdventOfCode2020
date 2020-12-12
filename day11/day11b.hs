import Data.List (find)
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
        n = length $ filter (==(Just '#')) xs
        xs = map (scanDirection m (r,c)) directions

solve :: Matrix Char -> Int
solve m
    | m == n    = countOccupied m
    | otherwise = solve n
    where
        n = run m

count e m = length $ filter (==e) $ toList $ m
countOccupied = count '#'

scanDirection m pos dir = find (\x -> x/= '.') ys
    where
        xs = takeWhile validPos $ tail $ iterate (succ' dir) pos
        ys = map (m!) xs
        validPos (r,c) = r > 0 && r <= (nrows m) && c > 0 && c <= (ncols m)

directions = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

succ' (r,c) (x,y) = (r+x,c+y)

main = do
    str <- getContents
    print $ solve $ buildMatrix $ lines $ str
