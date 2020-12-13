main = do
    input <- lines <$> readFile "input.txt"
    let timestamp = (read :: String -> Int) (input !! 0)
    let busIds = map (read :: String -> Int) $ filter (\x -> x /= "x") $ splitOn (input !! 1) ','
    let minWaitTimeLst = findEarliestBus busIds timestamp
    let minWaitTime = minimum minWaitTimeLst
    print $ day13p1 minWaitTime minWaitTimeLst busIds

day13p1 :: Int -> [Int] -> [Int] -> Int
day13p1 minWaitTime minWaitTimeLst busIds =
    let
       (x,y)  = head $ filter (\(x,y) -> x == minWaitTime) (zip minWaitTimeLst busIds)
    in
        x * y

findEarliestBus :: [Int] -> Int -> [Int]
findEarliestBus xs y = map func xs
    where
        func :: Int -> Int
        func x = 
            if mod y x == 0
            then 0
            else (x - (mod y x)) 

splitOn :: String -> Char -> [String]
splitOn xs c = go xs c [] []
go [] c acc out = reverse acc:out
go (x:ys) c acc out
    | x == c = go ys c [] (reverse acc:out)
    | otherwise = go ys c (x:acc) out