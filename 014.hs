import Data.Ord
import Data.List
import Data.Array

collatzArr :: Integer -> Array Integer Integer
collatzArr n = arr where 
    arr = (listArray (1, n)) (0 : map collatz [2 .. n])
    collatz x | y > n = 1 + collatz y
              | otherwise = 1 + (arr ! y)
              where y | even x = x `div` 2
                      | otherwise = 3 * x + 1

main :: IO ()
main = (print . fst . maximumBy (comparing snd) . assocs . collatzArr) 1000000