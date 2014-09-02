-- we reduce the problem into finding the list of 6 two-digit numbers such that
-- any two consecutive elements when concatenated yields a distinct polygonal

import Data.List
import Data.List.Ordered (member)
import Data.Array
import Data.Maybe
import Control.Monad (msum)

polygonals :: [[Int]]
polygonals = map (crop . scanl (+) 0 . diffList) [0..5] where
    diffList k = iterate (+ (k + 1)) 1
    crop = filter ((> 10) . (flip div 100)) . 
           filter ((> 10) . (flip mod 100)) . 
           takeWhile (< 10000) . dropWhile (< 1000)

isPolygonal :: Int -> Int -> Bool
isPolygonal k = flip member (polygonals !! k)

combine :: Int -> Int -> Int
combine prefix suffix = prefix * 100 + suffix

suffixes :: Int -> Int -> [Int]
suffixes k prefix = filter ((isPolygonal k) . (combine prefix)) [10..99]

suffixArr :: Array (Int, Int) [Int] 
suffixArr = listArray ((0, 10), (5, 99)) (map (uncurry suffixes) indices) where
    indices = [ (k, prefix) | k <- [0..5], prefix <- [10..99] ]

search :: Int -> [Int] -> Int -> Maybe [Int]
search first [k] prev = if isPolygonal k (combine prev first) then Just [prev]
                                                              else Nothing
search first (k:ks) prev = msum searchDeeper >>= return . (prev :) where 
    searchDeeper = map (search first ks) (suffixArr ! (k, prev))

mergeBack:: [Int] -> [Int]
mergeBack xs = go (head xs) (head xs) (tail xs) where
    go first prev [] = [ combine prev first ]
    go first prev (next:rest) = combine prev next : go first next rest

solve :: Maybe Int
solve = msum searchAll >>= return . sum . mergeBack where
    searchAll = [ search first ks first | first <- [10..99]
                                        , ks <- permutations [0..5] ]

main :: IO ()
main = print solve