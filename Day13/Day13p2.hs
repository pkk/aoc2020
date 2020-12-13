import Data.List.Split

main = do
    input <- lines <$> readFile "input.txt"
    let tuples = getTuples $ map getBusIdsMaybes $ splitOn "," (input !! 1)
    let start = (0, fst $ head $ tuples)
    print $ day13p2 (tail tuples) start

getBusIdsMaybes :: String -> Maybe Int
getBusIdsMaybes busId
    | busId == "x" = Nothing
    | otherwise = Just (read busId :: Int)

getTuples :: [Maybe Int] -> [(Int, Int)]
getTuples xs = map (\(Just a, b) -> (a,b)) $ filter (\a -> fst a /= Nothing) $ zip xs [0..]

day13p2 :: [(Int,Int)] -> (Int,Int) -> Int
day13p2 (x:[]) cur = fst $ consecmul cur x
day13p2 (x:xs) cur = day13p2 xs $ consecmul cur x

consecmul :: (Int,Int) -> (Int,Int) -> (Int,Int)
consecmul (target, curlcm) (num, offset) = (head [ x | x <- [target, target+curlcm..], (x+offset) `mod` num == 0], lcm curlcm num)