import Data.List

main :: IO ()
main = do
    input <- sort <$> map (read :: String -> Int) <$> lines <$> readFile "input.txt"
    print $ day10p1 $ input

day10p1 :: [Int] -> Int
day10p1 xs = 
    let
        (a,b,c) = foldl (\(x,y,z) ele -> if ele - x == 1 then (ele,y+1,z) else (ele,y,z+1)) (0,0,1) xs
    in
        c * b