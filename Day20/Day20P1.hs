import Data.List.Split
import Data.List
import Data.Function.Memoize
import Debug.Trace

data Block = Dot | Hash | Outside deriving (Eq)
data Grid = Grid {
    index :: Int,
    func :: ((Int, Int) -> Block)
}

data Dir = U | L | R | D

-- is always a square
size :: Grid -> Int
size (Grid i f) = size' f 

size' :: ((Int, Int) -> Block) -> Int
size' f = length (takeWhile (\x -> f (x, 0) /= Outside) [0..]) - 1 

instance Eq Grid where
    (==) g1 g2 = index g1 == index g2

instance Show Block where
    show Dot = "."
    show Hash = "#"

instance Read Grid where
    readsPrec _ str = 
        let (topRow:gridStr:[]) = splitOn (":\n") str
            gridList = (splitOn "\n" gridStr)
            toBlock '#' = Hash
            toBlock '.' = Dot
            f (x, y) = if (x >= 10 || y >= 10 || x < 0 || y < 0) then Outside else toBlock $ (gridList !! y) !! x in
        [(Grid (read $ drop 5 topRow) (memoize f), "")]

instance Show Grid where
    show (Grid i f) = show i ++ "\n" ++ unlines [[(head . show) (f (x, y)) | x <- [0..size' f]] | y <- [0..size' f]]

seaMonsterCoords = [(0, 1), (1, 2), (4, 2), (5, 1), (6, 1), (7, 2), (10, 2), (11, 1), (12, 1), (13, 2), (16, 2), (17, 1), (18, 1), (18, 0), (19, 1)]

main :: IO ()
main = do
    input <- splitAndReadFile "input.txt" "\n\n"
    let neighbors = map (\g -> (g, getNeighbors input g)) input
    let corners = filter ((== 2) . length . snd) neighbors
    print $ product $ map (index . fst) corners

getNeighbors :: [Grid] -> Grid -> [Grid]
getNeighbors grids grid = filter (\g -> grid /= g && isNeighbor g grid) grids

flipGrid :: Int -> Grid -> Grid
flipGrid max (Grid i f) = Grid i (\(x, y) -> f (max - x, y))

rotateGrid :: Int -> Grid -> Grid
rotateGrid max (Grid i f) = Grid i (\(x, y) -> f (y, max - x))

allChoices :: Grid -> [Grid]
allChoices g = 
    let r = rotateGrid 9
        fg = flipGrid 9 g
    in [g, r g, (r . r) g, (r . r . r) g, fg, r fg, (r . r) fg, (r . r . r) fg]

allChoicesGeneric :: Grid -> [Grid]
allChoicesGeneric g = 
    let r = rotateGrid (size g)
        fg = flipGrid (size g) g
    in [g, r g, (r . r) g, (r . r . r) g, fg, r fg, (r . r) fg, (r . r . r) fg]


isNeighbor :: Grid -> Grid -> Bool
isNeighbor g1 g2 = length (potentialURows (allChoices g1) `intersect` potentialURows (allChoices g2)) > 0
    where potentialURows = map (\g -> [(func g) (x, 0) | x <- [0..9]])

getEdge :: Dir -> Grid -> [Block]
getEdge U (Grid _ f) = [f (x, 0) | x <- [0..9]]
getEdge L (Grid _ f) = [f (0, y) | y <- [0..9]]
getEdge R (Grid _ f) = [f (9, y) | y <- [0..9]]
getEdge D (Grid _ f) = [f (x, 9) | x <- [0..9]]

allEdges :: Grid -> [[Block]]
allEdges grid = map (\f -> f grid) [getEdge U, getEdge L, getEdge R, getEdge D]


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