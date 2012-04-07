import Data.Array
import Data.List
import Data.Ord

memo n = arr where 
    arr = listArray (1, n) $ 0 : map len [2..n]
    len x | y > n = 1 + len y
          | otherwise = 1 + arr!y
          where y | even x = div x 2
                  | otherwise = 3 * x + 1

main = print . fst . maximumBy' (comparing snd) . assocs . memo $ 1000000
  where
    maxBy f x y | f x y == GT = x
                | otherwise = y
    maximumBy' f = foldl1' (maxBy f)
