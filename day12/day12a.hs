parse :: String -> ((Int,Int,Int)->(Int,Int,Int))
parse str
    | x == 'N' = move (0,-n)
    | x == 'S' = move (0,n)
    | x == 'W' = move (-n,0)
    | x == 'E' = move (n,0)
    | x == 'F' = forward n
    | x == 'L' = turn (-n)
    | x == 'R' = turn n
    where
        (x:xs) = str
        n = read xs :: Int

move (i,j) (x,y,b) = (x+i,y+j,b)

forward n (x,y,b)
    | b == 0    = (x,y-n,b)
    | b == 90   = (x+n,y,b)
    | b == 180  = (x,y+n,b)
    | b == 270  = (x-n,y,b)
    | otherwise = error "Illegal bearing"

turn n (x,y,b) = (x,y,b')
    where b' = (b+n) `mod` 360

solve :: [String] -> (Int,Int,Int) -> (Int,Int,Int)
solve [] y = y
solve (x:xs) y = solve xs $ parse x $ y

main = do
    str <- getContents
    let (x,y,_) = solve (lines str) (0,0,90)
    print $ (abs x) + (abs y)
