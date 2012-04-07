import Data.List hiding (union)
import Data.List.Ordered (union, minus)

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] $ map mults $ tail primes
    mults p = iterate (+ (2 * p)) (p * p)
    union' (x:xs) ys = x: union xs ys

nonSquared n = m * m /= n where m = round $ sqrt $ fromIntegral n

isOk n = all nonSquared $ map (\p -> div (n - p) 2) $ takeWhile (<= n) primes

main = print $ head $ filter isOk [9, 11..]