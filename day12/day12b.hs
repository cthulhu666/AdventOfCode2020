import Data.Matrix (fromList, toList)

--sample input
input = ["F10",
         "N3",
         "F7",
         "R90",
         "F11"]

rotations = [(90, fromList 2 2 [0,1,-1,0]),
             (180, fromList 2 2 [-1,0,0,-1]),
             (270, fromList 2 2 [0,-1,1,0])]

data World = World { waypointPosition :: (Int,Int),
                     shipPosition :: (Int,Int) }
    deriving (Show)

-- The waypoint starts 10 units east and 1 unit north relative to the ship.
-- The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.
start = World (10,1) (0,0)

moveShip :: Int -> World -> World
moveShip n (World (wx, wy) (sx, sy)) = World (wx, wy) (sx+dx, sy+dy)
    where
        dx = wx * n
        dy = wy * n

-- it assumes correct angle: -90,0,90,180,270 etc
rotateWaypoint :: Int -> World -> World
rotateWaypoint x world
    | y == 0    = world
    | otherwise = World (wx', wy') (shipPosition world)
    where
        y = x `mod` 360
        [wx', wy'] = toList $ rotationMatrix * (fromList 2 1 [wx, wy])
        (wx, wy) = waypointPosition world
        rotationMatrix = case lookup y rotations of
                            Just x -> x
                            Nothing -> error "Invalid rotation"


moveWaypoint :: (Int,Int) -> World -> World
moveWaypoint (dx, dy) (World (wx, wy) s) = World (wx+dx, wy+dy) s

parse str
    | x == 'N'  = moveWaypoint (0, n)
    | x == 'S'  = moveWaypoint (0, -n)
    | x == 'E'  = moveWaypoint (n, 0)
    | x == 'W'  = moveWaypoint (-n, 0)
    | x == 'L'  = rotateWaypoint (-n)
    | x == 'R'  = rotateWaypoint n
    | x == 'F'  = moveShip n
    where
        (x:xs) = str
        n = read xs :: Int

solve :: [String] -> World -> World
solve [] w = w
solve (x:xs) w = solve xs $ parse x $ w

main = do
    str <- getContents
    let (World _ (x,y)) = solve (lines str) start
    print $ (abs x) + (abs y)
