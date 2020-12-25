
import Math.NumberTheory.Powers.Modular


modulus :: Int
modulus = 20201227

loop :: Int -> [Int]
loop subjectNum = [powMod subjectNum n modulus | n <- [1..]]

loopSize :: Int -> Int -> Int
loopSize pubKey = (+1) . length . takeWhile (/= pubKey) . loop 


encKey :: Int -> Int -> Int
encKey loopSize = (!! (loopSize - 1)) . loop 


p1 :: Int -> Int -> Int -> Int -> Int
p1 doorPubKey cardPubKey doorSubjectNum cardSubjectNum = encKey doorLoopSize cardPubKey
    where
        doorLoopSize = loopSize doorPubKey doorSubjectNum
        cardLoopSize = loopSize cardPubKey cardSubjectNum

main :: IO ()
main = do
    print $ p1 6930903 19716708 7 7 