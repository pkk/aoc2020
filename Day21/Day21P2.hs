{-# LANGUAGE ScopedTypeVariables #-}
import Data.List.Split
import Data.List
import qualified Data.Map as M

data Food = Food {
    ingredients :: [String],
    allergens :: [String]
} deriving (Show)


instance Read Food where
    readsPrec _ recipe = 
        let (ingredients:allergens:[]) = splitOn " (" recipe
        in [(Food (splitOn " " ingredients) (map (filter $ \x -> not $ x `elem` ",)") $ tail $ splitOn " " allergens), "")]

main :: IO ()
main = do
    input <- splitAndReadFile "input.txt" "\n"
    let allergyMap =  run M.empty input
    let final = concat $ intersperse "," (map snd (sort $ run2 allergyMap))
    putStrLn final

run :: M.Map String [String] -> [Food] -> M.Map String [String]
run a2f ((Food ingredients allergens):foods) = run (updateMap a2f ingredients allergens) foods
    where updateMap a2f ingredients (allergen:allergens) = 
            let updated = 
                 case M.lookup allergen a2f of 
                    Just (ingredients') -> M.insert allergen (ingredients `intersect` ingredients') a2f
                    Nothing -> M.insert allergen ingredients a2f
            in updateMap updated ingredients allergens
          updateMap a2f ingredients [] = a2f
run a2f _ = a2f

run2 :: M.Map String [String] -> [(String, String)]
run2 a2f = run' a2f (M.toList a2f)
    where run' full ((allergen, foods):a2f) = 
           case foods of
            food:[] -> (allergen, food) : (run2 $ M.map (filter (/= food)) full)
            _ -> run' full a2f
          run' full [] = []


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