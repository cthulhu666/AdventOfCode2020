type Point = (Int, Int, Int) -- x,y,z
type Bounds = ((Int, Int), (Int, Int), (Int, Int))

data World = World Bounds [Point]
    deriving Show

parse :: String -> World
parse str = World bounds points
    where
        rows = lines str
        mapRow s = map fst $ filter (\(_,ch) -> ch=='#') $ enumerate s
        xs = map mapRow rows
        points = [(x,y,0) | (y, r) <- enumerate xs, x <- r]
        bounds = ((0, w-1), (0, h-1), (0, 0))
        w = length $ head rows
        h = length rows


cycle :: World -> Int -> World
cycle w 0 = w
cycle (World bounds points) n = Main.cycle newWorld (n-1)
    where
        newWorld = World newBounds newPoints
        newBounds = inflateBounds bounds
        newPoints = remainingActive ++ newActive
        xs = iterateInactive (World newBounds points)
        remainingActive = map fst $ filter (\(_, n) -> (n-1) == 2 || (n-1) == 3) $ map (activeNeighbours points) points
        newActive = map fst $ filter (\(_, n) -> n == 3) xs


inflateBounds :: Bounds -> Bounds
inflateBounds ((x1,x2), (y1,y2), (z1,z2)) = ((x1-1, x2+1), (y1-1, y2+1), (z1-1, z2+1))


iterateInactive :: World -> [(Point, Int)]
iterateInactive (World ((x1,x2), (y1,y2), (z1,z2)) points) = map (activeNeighbours points) ys
    where
        xs = [(x,y,z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
        ys = filter (\p -> not (p `elem` points)) xs


activeNeighbours :: [Point] -> Point -> (Point, Int)
activeNeighbours xs (x,y,z) = ((x,y,z), length ns)
    where ns = filter (\(x',y',z') -> (abs (x'-x)) <= 1 && (abs (y'-y)) <= 1 && (abs (z'-z)) <= 1) xs


enumerate :: [a] -> [(Int, a)]
enumerate xs = [0..] `zip` xs


solve :: World -> Int
solve w = length points
    where (World _ points) = Main.cycle w 6


main :: IO ()
main = do
    str <- getContents
    let world = parse str
    print $ solve world
