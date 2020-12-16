import Data.Bits
import Text.ParserCombinators.ReadP

--input = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n"
--        ++ "mem[8] = 11\n"
--        ++ "mem[7] = 101\n"
--        ++ "mem[8] = 0\n"

data World = World { mask   :: Mask,
                     memory :: [Mem] }
    deriving Show

type Mem = (Int, Int)
type Mask = String

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

-- this is terribly slow...
runMem :: Mem -> World -> World
runMem (addr, val) (World mask mem) = World mask newMem
    where
        newMem = (addr, applyMask mask val) : zs
        zs = filter (\(x, _) -> x /= addr) mem

applyMask :: Mask -> Int -> Int
applyMask mask val = apply ys val
    where
        apply [] v = v
        apply ((i,n):xs) v
            | n == '0'  = apply xs (clearBit v i)
            | n == '1'  = apply xs (setBit v i)
            | otherwise = apply xs v
        ys = zip [0..] $ reverse mask

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
