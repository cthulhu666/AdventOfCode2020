-- basically the same code as part A, but using IntMap
-- it certainly can be optimised further
-- also I suspect there is a cycle so it's not necessary to run all 30M iterations


import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.Strings (strSplitAll)

type TurnNumber = Int
type Memory = Map.IntMap [TurnNumber]

run :: Memory -> Int -> TurnNumber -> Int
run m n 30000001 = n
--run m n 2021 = n
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
    where l = Map.lookup n mem


update :: Memory -> Int -> TurnNumber -> Memory
update mem n m =
    case l of
        Just xs -> Map.insert n (m:xs) mem
        Nothing -> Map.insert n [m] mem
    where l = Map.lookup n mem


main = do
    str <- getContents
    let input = [read x | x <- strSplitAll "," str]
    let mem = Map.fromList $ map (\(x,y) -> (x, [y])) $ zip input [1..]
    print $ run mem (last input) (length input + 1)
