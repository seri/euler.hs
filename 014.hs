import Data.Array
import Data.List
import Data.Tuple

memo n = arr where 
    arr = listArray (1, n) $ 0 : map collatz [2..n]
    collatz x | y > n = 1 + collatz y
              | otherwise = 1 + arr!y
              where y | even x = div x 2
                      | otherwise = 3 * x + 1

main = print . snd . foldl1' max . map swap . assocs . memo $ 1000000