import Data.Strings (strSplitAll)

type TurnNumber = Int
type Memory = [(Int, [TurnNumber])]

run :: Memory -> Int -> TurnNumber -> Int
run m n 2021 = n
run m n i = run m' n' (i+1)
    where
        m' = update m n' i
        n' = check m n

check :: Memory -> Int -> Int
check mem n =
    case l of
        Just [x] -> 0
        Just xs -> foldl1 (-) $ take 2 xs
        Nothing -> 0
    where l = lookup n mem


update :: Memory -> Int -> TurnNumber -> Memory
update mem n m =
    case l of
        Just xs -> (n, m:xs) : filter (\(x,_) -> x /= n) mem
        Nothing -> (n, [m]) : mem
    where l = lookup n mem


main = do
    str <- getContents
    let input = [read x | x <- strSplitAll "," str]
    let mem = map (\(x,y) -> (x, [y])) $ zip input [1..] :: Memory
    print $ run mem (last input) (length input + 1)
