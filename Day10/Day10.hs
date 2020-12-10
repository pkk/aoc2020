main :: IO ()
main = do
    input <- map (read :: String -> Int) <$> lines <$> readFile "input.txt"
    print $ input !! 0