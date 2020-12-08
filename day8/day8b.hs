import Data.List (find, nub)
import Data.Strings (strReplace, strSplit)

-- acc, pc
data State = State Int Int
    deriving (Show)

-- halt, err (halt means program finished with a success)
data Ctrl = Ctrl Bool Bool
    deriving (Show)

type Program = [(State -> State)]

variants :: [(String, String)] -> [[(String, String)]]
variants xs = foldr f [] (zip [0..] xs)
    where
        f (n, _) a = ((take n xs) ++ switch (xs!!n) : drop (n+1) xs) : a
        switch ("nop", x) = ("jmp", x)
        switch ("jmp", x) = ("nop", x)
        switch (x, y) = (x, y)

parse :: (String, String) -> (State -> State)
parse (x, y) = case x of
                    "nop" -> nop z
                    "acc" -> acc z
                    "jmp" -> jmp z
               where z = read (strReplace "+" "" y) :: Int

nop :: Int -> State -> State
nop _ (State acc pc) = State acc (pc+1)

acc :: Int -> State -> State
acc x (State acc pc) = State (acc+x) (pc+1)

jmp :: Int -> State -> State
jmp x (State acc pc) = State acc (pc+x)

solve programs = find pred results
    where
        results = map f programs
        f p = run p (State 0 0) [] (Ctrl False False)
        pred ((State acc _), _, (Ctrl True False)) = True
        pred _ = False

run :: Program -> State -> [Int] -> Ctrl -> (State, [Int], Ctrl)
run program state trace (Ctrl True err) = (state, trace, Ctrl True err)
run program state trace (Ctrl halt True) = (state, trace, Ctrl halt True)
run program state trace (Ctrl False False) = run program newState (pc:trace) (Ctrl halt err)
    where
        newState = if halt then state else f state
        f = program!!(pc)
        (State _ pc) = state
        err = elem pc trace
        halt = pc == length program

main = do
    str <- getContents
    let source = map (strSplit " ") $ lines str
    print $ solve . map (map parse) . nub . variants $ source
