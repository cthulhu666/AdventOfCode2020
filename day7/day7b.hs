import Data.Strings (strSplit, strSplitAll)
import Text.Regex.Posix

data Color = Color String
    deriving (Show, Eq)

type Rule = (Color, [(Color, Int)])

myColor = Color "shiny gold"

parse :: String -> Rule
parse str = (c, f xs)
    where
        (x,y) = strSplit "contain" str
        Just (c, _) = parseColor x
        xs = sequence $ map parseColor (strSplitAll "," y)
        f (Just x) = x
        f Nothing = []

parseColor :: String -> Maybe (Color,Int)
parseColor str = if m == "no other" then Nothing else Just ((Color m), n)
    where
        m = (str =~ "([a-z][a-z ]+) bag" :: [[String]])!!0!!1
        n = read (str =~ "([0-9]+)" :: String) :: Int

solve :: [Rule] -> Int
solve rules = sum . map snd $ xs
    where
        xs = expand rules r
        Just r = lookup myColor rules

expand :: [Rule] -> [(Color, Int)] -> [(Color, Int)]
expand rules [] = []
expand rules xs = xs ++ (expand rules [x' | x <- y, x' <- x])
    where
        (Just y) = sequence $ map f xs
        f (x,n) = fmap (mult n) (lookup x rules)
        mult n xs = map (\(c,m) -> (c, n*m)) xs


main = do
    str <- getContents
    let rules = map parse $ lines str
    print $ solve rules
