import Data.List
import Data.Maybe
import Data.Char


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
        let p2ValidPassportList = filter isValidPassportP2 passports
        let p2ValidPassportCount = length p2ValidPassportList
        print p2ValidPassportCount

getGroupsOfStrings :: [String] -> [String] -> [[String]] -> [[String]]
getGroupsOfStrings [] _ acc = acc
getGroupsOfStrings ("":xs) curr acc = getGroupsOfStrings xs [] (curr:acc)
getGroupsOfStrings (x:xs) curr acc = getGroupsOfStrings xs ((words x)++curr) acc

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

getPassportEntry :: String -> [String] -> Maybe String
getPassportEntry key values = 
    if parsed == []
    then Nothing
    else 
        Just (tail (head parsed))
    where
        parsed = (catMaybes . map (stripPrefix key)) values

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

isValidPassportP2 :: Passport -> Bool
isValidPassportP2 Passport {
    byr = Just byr,
    iyr = Just iyr,
    eyr = Just eyr,
    hgt = Just hgt,
    hcl = Just hcl,
    ecl = Just ecl,
    pid = Just pid,
    cid = _ }
    | all (== True) [validateByr byr, validateIyr iyr, validateEyr eyr, validateHgt hgt, validateHcl hcl, validateEcl ecl, validatePid pid]
    = True
isValidPassportP2 _ = False

validateByr year = length year == 4 && all isDigit year && (1920 <= read year) && (read year <= 2002)
validateIyr year = length year == 4 && all isDigit year && (2010 <= read year) && (read year <= 2020)
validateEyr year = length year == 4 && all isDigit year && (2020 <= read year) && (read year <= 2030)
validateHgt height = ((dropWhile isDigit height) == "cm" && 150 <= (read number) && (read number) <= 193)
					|| ((dropWhile isDigit height) == "in" && 59 <= (read number) && (read number) <= 76)
					where number = takeWhile isDigit height
validateHcl color = head color == '#' && all (\ x -> isDigit x || isBetweenAF x) (tail color)
					where isBetweenAF letter = (ord 'a') <= (ord letter) && (ord letter) <= (ord 'f')
validateEcl color = any (==color) ["amb","blu","brn","gry","grn","hzl","oth"]
validatePid pid = all isDigit pid && length (filter isDigit pid) == 9