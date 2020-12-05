module Day1 where


getLines :: FilePath -> IO [Int]
getLines path = do contents <- readFile path
                   return (map (read :: String -> Int) (lines contents))

expense_report = getLines "input"

part1 :: [Int] -> Int
part1 xs = head [ x * y| x <- xs, y <- xs, x + y == 2020]

part2 :: [Int] -> Int
part2 xs = head [ x * y * z| x <- xs, y <- xs, z <- xs, x + y + z == 2020]

part1ans = fmap part1 expense_report
part2ans = fmap part2 expense_report