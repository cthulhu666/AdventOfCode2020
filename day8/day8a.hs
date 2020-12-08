import Data.Strings (strReplace, strSplit)

-- acc, pc
data State = State Int Int
    deriving (Show)

type Program = [(State -> State)]

parse :: String -> (State -> State)
parse str = case x of
                "nop" -> nop z
                "acc" -> acc z
                "jmp" -> jmp z
            where
                (x, y) = strSplit " " str
                z = read (strReplace "+" "" y) :: Int

nop :: Int -> State -> State
nop _ (State acc pc) = State acc (pc+1)

acc :: Int -> State -> State
acc x (State acc pc) = State (acc+x) (pc+1)

jmp :: Int -> State -> State
jmp x (State acc pc) = State acc (pc+x)

run :: Program -> State -> [Int] -> Bool -> (State, [Int], Bool)
run program state trace True = (state, trace, True)
run program state trace False = run program newState (pc:trace) halt
    where
        newState = if halt then state else f state
        f = program!!(pc)
        (State _ pc) = state
        halt = elem pc trace

main = do
    str <- getContents
    let program = map parse $ lines str
    print $ run program (State 0 0) [] False
