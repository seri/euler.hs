-- Haskell implementation of the elegant approach described here:
-- http://www.mathblog.dk/project-euler-59-xor-encryption/

import Data.Char
import Data.Bits
import Data.Ord
import Data.List

decrypt :: [Int] -> [Int] -> [Int]
decrypt message key = zipWith xor message (cycle key)

splitUp :: [a] -> [[a]]
splitUp xs = map (map (xs !!)) indices where
    indices = map (takeWhile (< length xs) . iterate (+ 3)) [0, 1, 2]

mostFrequent :: Ord a => [a] -> a
mostFrequent = head . maximumBy (comparing length) . group . sort

searchKey :: [Int] -> [Int]
searchKey message = map searchKey' (splitUp message) where
    searchKey' = xor 32 . mostFrequent

solve :: [Int] -> Int
solve message = sum . decrypt message . searchKey $ message

getMessage :: FilePath -> IO [Int]
getMessage file = readFile file >>= return . readInts where
    readInts content = read (concat [ "[", content, "]" ]) :: [Int]

main :: IO ()
main = getMessage "059.in" >>= print . solve