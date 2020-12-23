import Text.Regex.Posix
import Data.Strings (strSplitAll)

type Range = (Int,Int)

parseNearbyTickets :: String -> Maybe [[Int]]
parseNearbyTickets str =
    case m of
      [[_, s]]  ->      Just [[read x | x <- strSplitAll "," y] | y <- lines s]
      otherwise ->      Nothing
    where
        m = str =~ "nearby tickets:\n([0-9,\n]+)" :: [[String]]


parseFields :: String -> Maybe [(String,Range,Range)]
parseFields str = sequence $ map f m
    where
        m = str =~ "^([a-z]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)" :: [[String]]
        f [_,s,a,b,c,d] = Just (s,(read a, read b),(read c, read d))
        f _ = Nothing


validate :: [(String, Range, Range)] -> [Int] -> [Int]
validate fields ticket = filter f ticket
    where
        f n = all (\(_,r1,r2) -> not (inRange r1 n || inRange r2 n)) fields
        inRange (n,m) x = n <= x && x <= m


solve (Just tickets) (Just fields) = sum . concat . map (validate fields) $ tickets


main = do
    str <- getContents
    let tickets = parseNearbyTickets str
    let fields = parseFields str
    print $ solve tickets fields
