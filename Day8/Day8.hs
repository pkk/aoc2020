data Instruction = ACC | JMP | NOP deriving (Show)

data Code = Code {
    instruction :: Instruction,
    offset :: Int
} deriving (Show)

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let code = map parseCode input
    print $ day8p1 $ code
    print $ day8p2 $ code


day8p1 :: [Code] -> Int
day8p1 xs = go xs [] 0 0
    where
        go  :: [Code] -> [Int] -> Int -> Int -> Int
        go xs lines off acc = 
            if off `elem` lines
            then acc
            else 
                let
                    ele = xs !! off
                    jump = offset ele
                in
                    case instruction ele of
                        ACC -> go xs (off:lines) (off + 1) (acc + jump)
                        JMP -> go xs (off:lines) (off + jump) acc
                        NOP -> go xs (off:lines) (off + 1) acc

day8p2 :: [Code] -> Int
day8p2 xs = go xs xs [] 0 0 0
    where
        go origCodeList modifiedCodeList seenLines currLine acc flipLoc
            | currLine `elem` seenLines =
                go origCodeList (flipInstruction flipLoc origCodeList) [] 0 0 (flipLoc + 1)
            | currLine == length origCodeList = acc
            | otherwise = 
                let
                    ele = modifiedCodeList !! currLine
                    jump = offset ele
                in
                    case instruction ele of
                        NOP -> go origCodeList modifiedCodeList (currLine : seenLines) (currLine + 1) acc flipLoc
                        JMP -> go origCodeList modifiedCodeList (currLine : seenLines) (currLine + jump) acc flipLoc
                        ACC -> go origCodeList modifiedCodeList (currLine : seenLines) (currLine + 1) (acc + jump) flipLoc

flipInstruction :: Int -> [Code] -> [Code]
flipInstruction count = go 0
    where
        --go :: Int -> [Code] -> [Code]
        go _ [] = []
        go idx (x:xs)
            | idx == count = flipCode x : xs
            | otherwise = x : go (idx + 1) xs

flipCode :: Code -> Code
flipCode code = 
    let
        jump = offset code
        nInst = case (instruction code) of
            ACC -> ACC
            JMP -> NOP
            NOP -> JMP
    in
        Code {
            instruction = nInst,
            offset = jump
        }

parseCode :: String -> Code
parseCode code = 
    let
        ws = words code
        inst = case (head ws) of 
            "acc" -> ACC
            "jmp" -> JMP
            _ -> NOP
        jumpBy = 
            let
                jumpRaw = head (tail ws)
                jumpNumStr = 
                    if head jumpRaw == '+'
                    then tail jumpRaw
                    else jumpRaw
            in
                (read :: String -> Int) jumpNumStr
       
    in
        Code {
            instruction = inst,
            offset = jumpBy
            
        } 