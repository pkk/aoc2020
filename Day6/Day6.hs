main :: IO ()
main = do
    input <- readFile "input.txt"
    let lst = lines input
    let groups = getGroups lst
    let yesCountPerGroup = map getYesCountPerGroup groups
    let p6d1 = sum yesCountPerGroup
    let answeredByAllCountLst = map getAnsweredByAll groups
    let p6d2 = sum (map getAnsweredByAll groups)
    print p6d1
    print p6d2



getAnsweredByAll :: [String] -> Int
getAnsweredByAll lst = 
    let 
        go [] ys acc = acc
        go (x:xs) ys acc = if (x `elem` ys) then go xs ys (x:acc) else go xs ys acc
        commonLetters :: [Char] -> [Char]  -> [Char]
        commonLetters xs ys = go xs ys []
        answersLst = foldl (\acc ele -> commonLetters acc ele) "abcdefghijklmnopqrstuvwxyz" lst
    in
        length answersLst


getGroups :: [String] -> [[String]]
getGroups lines = go lines [] []
go [] curr acc = curr:acc
go ("":xs) curr acc = go xs [] (curr:acc)
go (x:xs) curr acc = go xs (x:curr) acc

getYesCountPerGroup :: [String] -> Int
getYesCountPerGroup lst = 
    let
        foldWord :: [Char] -> String -> [Char]
        foldWord lookUp = foldl (\acc ele -> if ele `elem` acc then acc else (ele:acc)) lookUp
        foldWords lookUp = foldl (\acc word -> foldWord acc word) []
    in
        length $ foldWords [] lst
