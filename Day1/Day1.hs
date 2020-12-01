module Day1 where

getWords :: FilePath -> IO [Int]
getWords path = do contents <- readFile path
                   return (map (read :: String -> Int) (lines contents))

expense_report = getWords "input"

part1 (x : xs) = if elem (2020 - x) xs then x * (2020-x) else part1 xs

ans1 = fmap part1 expense_report