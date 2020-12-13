import Data.List 

main :: IO ()
main = do
    input <- map (read :: String -> Integer) <$> lines <$> readFile "input.txt"
    let day9p1ans = day9p1 $ input
    let day9p2ans = sumFirstAndLast $ day9p2 input day9p1ans
    print day9p1ans
    print day9p2ans

day9p1 :: [Integer] -> Integer
day9p1 xs = go xs [] 0
    where
        go :: [Integer] -> [Integer] -> Integer -> Integer
        go [] lst acc = acc
        go (x:xs) lst acc =
            if (length lst < 25)
            then go xs (x:lst) acc
            else
                let
                    currLst = take 25 lst
                    ans = length [(a,b)| a <- currLst, b <- currLst, a + b == x]
                in
                    if ans == 0 then x else go xs (x:lst) acc

day9p2 :: [Integer] -> Integer -> [Integer]
day9p2 xs target =
    let
        targetLstSize = length $ takeWhile (<= target) (scanl1 (+) xs)
        targetLst = take targetLstSize xs
    in
        if sum targetLst == target && length targetLst >= 2
        then sort targetLst
        else day9p2 (drop 1 xs) target

sumFirstAndLast :: [Integer] -> Integer
sumFirstAndLast xs = xs !! 0 + xs !! (length xs - 1)