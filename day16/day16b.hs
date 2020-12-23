import Data.List (isPrefixOf, partition, transpose)
import Data.Strings (strSplitAll)
import Text.Regex.Posix

type Range = (Int,Int)
type Ticket = [Int]
type Field = (String, Range, Range)

parseMyTicket :: String -> Maybe [Int]
parseMyTicket str =
    case m of
        [[_, s]]  ->    Just [read x | x <- strSplitAll "," s]
        otherwise ->    Nothing
    where m = str =~ "your ticket:\n([0-9,]+)" :: [[String]]


parseNearbyTickets :: String -> Maybe [[Int]]
parseNearbyTickets str =
    case m of
      [[_, s]]  ->      Just [[read x | x <- strSplitAll "," y] | y <- lines s]
      otherwise ->      Nothing
    where
        m = str =~ "nearby tickets:\n([0-9,\n]+)" :: [[String]]


parseFields :: String -> Maybe [Field]
parseFields str = sequence $ map f m
    where
        m = str =~ "^([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" :: [[String]]
        f [_,s,a,b,c,d] = Just (s,(read a, read b),(read c, read d))
        f _ = Nothing


validate :: [Field] -> Ticket -> (Ticket, Bool)
validate fields ticket = (ticket, not $ any f ticket)
    where
        f n = all (\(_,r1,r2) -> not (inRange r1 n || inRange r2 n)) fields


inRange :: (Int,Int) -> Int -> Bool
inRange (n,m) x = n <= x && x <= m


validateCol :: [Field] -> [Int] -> [String]
validateCol fields xs = map (\(x,_,_) -> x) . filter valid $ fields
    where valid (_,r1,r2) = all (\x -> inRange r1 x || inRange r2 x) xs


assignFields :: [[String]] -> [(Int, String)]
assignFields [] = []
assignFields xs
    | length ys == 0    = []
    | length ys == 1    = (i, head j) : assignFields zs
    | otherwise         = error "Ambiguous"
    where
        ys = filter (\(_,x) -> length x == 1) $ zip [0..] xs
        zs = map (\x -> del (head . snd . head $ ys) x) xs
        (i,j) = head ys
        del s xs = snd $ partition (==s) xs

solve (Just myTicket) (Just tickets) (Just fields) = foldr1 (*) $ map (\(i,_) -> myTicket!!i) departureFields
    where
        validTickets = map fst $ filter snd $ map (validate fields) tickets
        cols = transpose $ myTicket : validTickets
        xs = map (validateCol fields) cols
        departureFields = filter (\(_,y) -> "departure" `isPrefixOf` y) $ assignFields xs


main = do
    str <- getContents
    let myTicket = parseMyTicket str
    let tickets = parseNearbyTickets str
    let fields = parseFields str
    print $ solve myTicket tickets fields
