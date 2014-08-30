-- My naive solution. It is verbose and inefficient but it works and it is
-- certainly easy to understand.

import Data.Char (toUpper, chr, ord)
import Data.Bits (xor)
import qualified Data.Set as Set

type Dictionary = Set.Set String


decrypt :: [Int] -> [Int] -> String
decrypt message key = zipWith convert message (cycle key) where
    convert x y = chr (x `xor` y)

generateKeys :: [[Int]]
generateKeys = [ [x, y, z] | x <- range, y <- range, z <- range ] where
    range = [97..122]


validate :: String -> Dictionary -> Bool
validate message dict = (sanityCheck allWords) && (length invalids < 10) where
    allWords = words message
    sanityCheck = all ((< 20) . length)
    invalids = take 10 . takeWhile isInvalid $ allWords
    isInvalid = not . flip Set.member dict . map toUpper

solve :: [Int] -> Dictionary -> Int
solve message dict = head answers where
    answers = [ sumAscii output | key <- generateKeys 
                                , let output = decrypt message key 
                                , validate output dict ]
    sumAscii = sum . map ord


buildDict :: FilePath -> IO Dictionary
buildDict file = readFile file >>= return . Set.fromList . readWords where
    readWords content = read content :: [String]

getMessage :: FilePath -> IO [Int]
getMessage file = readFile file >>= return . readInts where
    readInts content = read content :: [Int]

main :: IO ()
main = do
    dict <- buildDict "words.txt"
    message <- getMessage "059.in"
    print $ solve message dict