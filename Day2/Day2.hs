import Data.Text as T

--- presume this get the min-max
getMinMax txt = 
    let 
        limits = T.splitOn (T.pack "-") txt
        min = (Prelude.read :: String -> Int) (T.unpack (Prelude.head limits))
        max = (Prelude.read :: String -> Int) (T.unpack (Prelude.head (Prelude.tail limits)))
    in
        (min,max)

part1a :: Int -> Int -> String -> Char -> Bool
part1a min max password charToCheck =
    let 
        count = Prelude.length (Prelude.filter (\x -> x == charToCheck) password)
    in
        count >= min && count <= max

part2a :: Int -> Int -> String -> Char -> Bool
part2a min max password charToCheck = 
    let
        t1 = password !! (min - 1) == charToCheck 
        t2 = password !! (max - 1) == charToCheck
        myxor :: Bool -> Bool -> Bool
        myxor True False = True
        myxor False True = True
        myxor _ _ = False
    in
        myxor t1 t2


filterValidPwds :: (Int -> Int -> String -> Char -> Bool) ->[String] -> Int
filterValidPwds day2Func = Prelude.foldl (\acc ele -> if isValidPwd ele then acc + 1 else acc) 0
    where 
        isValidPwd :: String -> Bool
        isValidPwd pwd =
            let
                splitText = T.splitOn (T.pack " ") (T.pack pwd)
                minMaxTuple = getMinMax (Prelude.head splitText)
                min = fst minMaxTuple
                max = snd minMaxTuple
                charToCheck = Prelude.head (T.unpack (Prelude.head (T.splitOn (T.pack ":") (Prelude.head (Prelude.tail splitText)))))
                password = T.unpack (Prelude.head (Prelude.tail (Prelude.tail splitText)))
            in
                day2Func min max password charToCheck
            

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let lst = Prelude.lines contents
    putStrLn (show (filterValidPwds part1a lst))
    putStrLn (show (filterValidPwds part2a lst))
