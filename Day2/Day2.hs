import Data.Text as T

--- presume this get the min-max
getMinMax txt = 
    let 
        limits = T.splitOn (T.pack "-") txt
        min = (Prelude.read :: String -> Int) (T.unpack (Prelude.head limits))
        max = (Prelude.read :: String -> Int) (T.unpack (Prelude.head (Prelude.tail limits)))
    in
        (min,max)

filterValidPwds :: [String] -> Int
filterValidPwds = Prelude.foldl (\acc ele -> if isValidPwd ele then acc + 1 else acc) 0
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
                count_of_chars = Prelude.length (Prelude.filter (\x -> x == charToCheck) password)
            in
                count_of_chars >= min && count_of_chars <= max
            

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let lst = Prelude.lines contents
    let filteredList = filterValidPwds lst
    putStrLn (show filteredList)
