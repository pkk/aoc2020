import Data.List

main :: IO ()
main = do
        input <- readFile $ "input.txt"
        let lst = lines input
        let seatIds = map toSeatId (map convertToSeatNumber lst)
        let day5p1Ans = day5p1 seatIds
        let day5p2Ans = day5p2 (sort seatIds)
        print ("Day 5 Part 1 == " ++ show day5p1Ans)
        print ("Day 5 Part 2 == " ++ show day5p2Ans)

day5p1 :: [Int] -> Int
day5p1 = maximum

day5p2 :: [Int] -> Int
day5p2 seatIds = findSeatId (zip seatIds (tail seatIds))
    where
        findSeatId (x:xs)
            | fst x + 1 == snd x = findSeatId xs
            | otherwise = fst x + 1

toSeatId :: (Int,Int) -> Int
toSeatId (x,y) = y + ( x * 8)

convertToSeatNumber :: String -> (Int, Int)
convertToSeatNumber xs = 
    let 
        rowStr = take 7 xs
        seatStr = drop 7 xs

        (lowRow, hiRow) = foldl search (0,127) rowStr
        (lowSeat, hiSeat) = foldl search (0,7) seatStr

        search (lo,hi) 'F' = (lo, div (lo + hi) 2)
        search (lo,hi) 'L' = (lo, div (lo + hi) 2)
        search (lo,hi) 'B' = (1 + div (lo + hi) 2, hi)
        search (lo,hi) 'R' = (1 + div (lo + hi) 2, hi)

        actualRow = if last rowStr == 'F' then lowRow else hiRow
        actualSeat = if last seatStr == 'R' then hiSeat else lowSeat
    in
        (actualRow, actualSeat)