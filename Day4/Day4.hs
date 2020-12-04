import Data.List
import Data.Maybe


data Passport = Passport {
    byr :: Maybe String, -- Birth Year
    iyr :: Maybe String, -- Issue Year 
    eyr :: Maybe String, -- Expiration Year
    hgt :: Maybe String, -- Height
    hcl :: Maybe String, -- Hair Color
    ecl :: Maybe String, -- Eye Color
    pid :: Maybe String, -- Password ID
    cid :: Maybe String -- Country ID
} deriving (Show, Read, Eq)

main :: IO ()
main = do
        input <- readFile "input.txt"
        let rawPassportStrList = getGroupsOfStrings (lines input) [] [[]]
        let passports = map parsePassport rawPassportStrList
        let validPassportsLst = filter isValidPassport passports
        let validPassportCount = length validPassportsLst
        print validPassportCount

isValidPassport :: Passport -> Bool
isValidPassport Passport {
    byr = Just _,
    iyr = Just _,
    eyr = Just _,
    hgt = Just _,
    hcl = Just _,
    ecl = Just _,
    pid = Just _,
    cid = _ }
    = True
isValidPassport _ = False


getPassportEntry :: String -> [String] -> Maybe String
getPassportEntry key values = 
    if parsed == []
    then Nothing
    else 
        Just (tail (head parsed))
    where
        parsed = (catMaybes . map (stripPrefix key)) values

parsePassport :: [String] -> Passport
parsePassport rawData = 
    Passport {
        byr = getPassportEntry "byr" rawData,
        iyr = getPassportEntry "iyr" rawData,
        eyr = getPassportEntry "eyr" rawData,
        hgt = getPassportEntry "hgt" rawData,
        hcl = getPassportEntry "hcl" rawData,
        ecl = getPassportEntry "ecl" rawData,
        pid = getPassportEntry "pid" rawData,
        cid = getPassportEntry "cid" rawData
    }

getGroupsOfStrings :: [String] -> [String] -> [[String]] -> [[String]]
getGroupsOfStrings [] _ acc = acc
getGroupsOfStrings ("":xs) curr acc = getGroupsOfStrings xs [] (curr:acc)
getGroupsOfStrings (x:xs) curr acc = getGroupsOfStrings xs ((words x)++curr) acc
