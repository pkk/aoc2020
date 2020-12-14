import Data.List
import Data.Char
import Data.Bits
import qualified Data.IntMap.Strict as IM


main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let progs = parseProgs $ chunkMask input
    print $ day14p2 progs
    

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

day14p2 :: [Prog] -> Int
day14p2 progs = sum (IM.elems (foldl' go IM.empty progs))
  where
    go imap (Prog mask mems) = foldl' compute imap mems
      where
        compute imap' (Mem k v) =
          foldr (flip IM.insert v) imap'
            (getAddrs mask k)

getAddrs :: String -> Int -> [Int]
getAddrs mask addr =
  uncurry processFloats
    (foldl go (addr, mempty)
       (zip [0..] (reverse mask)))
    where
      go (addr',floats) (idx, 'X') = (addr', idx:floats)
      go (addr',floats) (idx, '1') = (addr' `setBit` idx, floats)
      go (addr',floats) (idx, '0') = (addr', floats)

processFloats :: Int -> [Int] -> [Int]
processFloats addr floatIdxs = applyTransform addr <$> transforms
  where
    transforms :: [[Int -> Int]]
    transforms = fmap make combos

    combos :: [[Int]]
    combos = combinations (length floatIdxs)

    applyTransform :: Int -> [Int -> Int] -> Int
    applyTransform = foldl (flip ($))
    make combo =
      fmap (\(idx, n) ->
              if n == 0
              then (`clearBit` idx)
              else (`setBit` idx))
      (zip floatIdxs combo)

combinations
  :: Int
  -> [[Int]]
combinations x =
  fmap toBits [0 .. (2^x) - 1 ]
    where
      toBits :: Int -> [Int]
      toBits n
        = take x
        $ zipWith (\idx b -> popCount (b `testBit` idx))
            [0..] (repeat n)
