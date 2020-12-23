import Data.List.Split
import qualified Data.Set as S

type Deck = [Int]
data Player = P1 | P2 deriving (Eq)

main :: IO ()
main = do
    (p1:p2:[]) <- splitFile "input.txt" "\n\n"
    let f p = (map read) $ tail $ filter (/= "") (splitOn "\n" p) :: [Int]
    let (deck1, deck2) = (f p1, f p2)
    let winningDeck = play deck1 deck2
    print $ score winningDeck

play :: Deck -> Deck -> Deck
play (x:xs) (y:ys)
    | x > y = play (xs ++ [x,y]) ys
    | y > x = play xs (ys ++ [y,x])
play [] ys = ys
play xs [] = xs

score :: Deck -> Int
score deck = sum $ map (\(x, y) -> x * y) (zip [1..] (reverse deck))

splitFile :: String -> String -> IO [String]
splitFile name splitter = do
        input <- readFile name
        let filtered = filter (\x -> x /= "") $ splitOn splitter input
        return filtered

splitAndReadFile :: Read a => String -> String -> IO [a]
splitAndReadFile name splitter = do
        input <- readFile name
        let filtered = filter (\x -> x /= "") $ splitOn splitter input
        return $ map read filtered