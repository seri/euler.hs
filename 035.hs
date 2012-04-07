import Data.Array
import Data.List hiding (union)
import Data.List.Ordered (union, minus)

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] $ map mults $ tail primes
    mults p = iterate (+ (2 * p)) (p * p)
    union' (x:xs) ys = x: union xs ys

limit = 10^6

goodPrimes = takeWhile (< limit) primes

primeMemo = listArray (0, limit) (repeat False) //
            zip goodPrimes (repeat True)

rotations xs = map rotate [1..(length xs)] where
    rotate n = right ++ left where (left, right) = splitAt n xs

isCircular n = all ((primeMemo !) . read) $ init $ rotations $ show n

circulars = filter isCircular goodPrimes

main = print $ length circulars

