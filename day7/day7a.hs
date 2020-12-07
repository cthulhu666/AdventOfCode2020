import Data.Set (fromList)
import Data.Strings (strSplit, strSplitAll)
import Text.Regex.Posix

data Color = Color String
    deriving (Show, Eq, Ord)

type Rule = (Color, [Color])

myColor = Color "shiny gold"

parse :: String -> Rule
parse str = (c, f xs)
    where
        (x,y) = strSplit "contain" str
        Just c = parseColor x
        xs = sequence $ map parseColor (strSplitAll "," y)
        f (Just x) = x
        f Nothing = []

parseColor :: String -> Maybe Color
parseColor str = if m == "no other" then Nothing else Just $ Color m
    where m = (str =~ "([a-z][a-z ]+) bag" :: [[String]])!!0!!1

solve :: [Rule] -> Int
solve rules = length $ filter id $ map (elem myColor) xs
    where xs = map (expand rules) (map snd rules)

expand :: [Rule] -> [Color] -> [Color]
expand rules [] = []
expand rules xs = xs ++ (expand rules [x' | x <- y, x' <- x])
    where
        (Just y) = sequence $ map f xs
        f x = lookup x rules

main = do
    str <- getContents
    let rules = map parse $ lines str
    print $ solve rules
