import Data.List
import Data.Char
import Data.Bits
import qualified Data.IntMap.Strict as IM


main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let progs = parseProgs $ chunkMask input
    print $ day14p1 progs
    

chunkMask :: [String] -> [[String]]
chunkMask [] = []
chunkMask (x:xs) 
    | "mask" `isPrefixOf` x = 
        let 
            more = takeWhile (not . isPrefixOf "mask") xs
            rest = dropWhile (not . isPrefixOf "mask") xs
        in
            (x:more) : chunkMask rest
    | otherwise = []

data Prog = Prog {
    mask :: String,
    mems :: [Mem]
} deriving (Show, Eq)

data Mem = Mem Int Int deriving (Show, Eq)

parseProgs :: [[String]] -> [Prog]
parseProgs = map go
    where
        go :: [String] -> Prog
        go (mask: mems) = 
            Prog (drop 7 mask) (map parseMem mems)
        parseMem :: String -> Mem
        parseMem xs = 
            let
                addr = (read :: String -> Int) $ takeWhile isDigit (drop 4 xs)
                val = (read :: String -> Int) . reverse . takeWhile isDigit . reverse $ xs
            in
                Mem addr val

day14p1 :: [Prog] -> Int
day14p1 progs = sum (IM.elems (foldl' go IM.empty progs))
  where
    go imap (Prog mask mems) = do
      foldl' compute imap mems
        where
          compute imap' (Mem k v) =
            IM.insert k (maskedValue mask v) imap'

maskedValue :: String -> Int -> Int
maskedValue mask num = do
  foldl' go num (zip [0..] (reverse mask))
    where
      go num (idx, 'X') = num
      go num (idx, '1') = num `setBit` idx
      go num (idx, '0') = num `clearBit` idx