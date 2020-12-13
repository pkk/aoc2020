import Data.List

main :: IO ()
main = do
    input <- sort <$> map (read :: String -> Integer) <$> lines <$> readFile "input.txt"
    print $ day10p1 $ input
    print $ day10p2 $ input

day10p1 :: [Integer] -> Integer
day10p1 xs = 
    let
        (_,b,c) = foldl (\(x,y,z) ele -> if ele - x == 1 then (ele,y+1,z) else (ele,y,z+1)) (0,0,1) xs
    in
        c * b

day10p2 :: [Integer] -> Integer
-- Take diffs, group, remove all 3's, get length of groups, convert tribonacci, multiply
day10p2 input = (product . map (tribonacci . length) . filter ((==1) . head) . group . diffs) input where
  tribonacci x = [1,2,4,7] !! (x-1) -- First numbers of Tribonacci (https://oeis.org/A000073)

diffs :: (Num a, Ord a) => [a] -> [a]
diffs input = zipWith (-) (tail full) full where
  sorted = sort input
  full = 0 : sorted ++ [3 + last sorted]