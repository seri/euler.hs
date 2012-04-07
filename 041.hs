import Data.Array
import Data.List hiding (union)
import Data.List.Ordered (union, minus)

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] $ map mults $ tail primes
    mults p = iterate (+ (2 * p)) (p * p)
    union' (x:xs) ys = x: union xs ys

isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) primes where
    m = round $ sqrt $ fromIntegral n

toNumber = foldl (\p q -> 10 * p + q) 0

main = print $ head $ filter isPrime $ sortBy (flip compare) $
       map toNumber $ (permutations [1..4]) ++ (permutations [1..7])