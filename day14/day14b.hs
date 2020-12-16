-- It takes ~6 mins to finish, so it would be great to revisit this with the profiler.

import Data.Bits
import Data.Char (digitToInt)
import Data.List (foldl')
import Text.ParserCombinators.ReadP

--input = "mask = 000000000000000000000000000000X1001X\n"
--        ++ "mem[42] = 100\n"
--        ++ "mask = 00000000000000000000000000000000X0XX\n"
--        ++ "mem[26] = 1\n"

data World = World { mask   :: Mask,
                     memory :: [Mem] }
    deriving Show

type Mem = (Addr, Int)
type Mask = String
type Addr = Int

parseMask :: ReadP Mask
parseMask = do
    string "mask = "
    mask <- count 36 $ satisfy (\ch -> elem ch "01X" )
    return $ mask

parseMem :: ReadP Mem
parseMem = do
    string "mem["
    addr <- many1 digit
    string "] = "
    val <- many1 $ digit
    eof
    return $ (read addr, read val)

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

runMask :: Mask -> World -> World
runMask x (World mask mem) = World x mem


runMem :: Mem -> World -> World
runMem (addr, val) (World mask mem) = World mask newMem
    where
        newMem = updateMem mem $ zip addrs $ repeat val
        addrs = map toDec $ expandMask $ applyMask mask addr


updateMem :: [Mem] -> [Mem] -> [Mem]
updateMem mem updates = foldl f mem updates
    where
        f a x = x : filter (\(z, _) -> z /= fst x) a


expandMask :: Mask -> [Mask]
expandMask m = expand $ break (=='X') m
    where
        expand (n, "") = [n]
        expand (n,(_:ms)) = expandMask x ++ expandMask y
            where
                x = (n ++ "0" ++ ms)
                y = (n ++ "1" ++ ms)

applyMask :: Mask -> Addr -> Mask
applyMask m a = reverse $ map f xs
    where
        xs = zip [0..] $ reverse m
        f (_, 'X') = 'X'
        f (i, '0') = if testBit a i then '1' else '0'
        f (_, '1') = '1'

toDec :: Mask -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

-- I have no idea whatsoever how to use ReadP...
parseLine :: String -> (World -> World)
parseLine str
    | length xs == 1  = runMask $ fst $ xs!!0
    | length ys == 1  = runMem $ fst $ ys!!0
    where
        xs = readP_to_S parseMask str
        ys = readP_to_S parseMem str

solve [] w = w
solve (f:fs) w = solve fs $ f w

main = do
    str <- getContents
    let fs = map parseLine $ lines str
    let w = World "" []
    print $ foldr1 (+) $ map snd $ memory $ solve fs w
