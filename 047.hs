import Data.List hiding (union)
import Data.List.Ordered (union, minus)

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] $ map mults $ tail primes
    mults p = iterate (+ (2 * p)) (p * p)
    union' (x:xs) ys = x: union xs ys

factorize ps@(p:pr) n | n <= 1 = []
                      | p * p > n = [n]
                      | r == 0 = p : factorize ps q
                      | otherwise = factorize pr n
                      where (q, r) = divMod n p

isOk = all $ (==4) . length . group . factorize primes

main = print $ head $ head $ filter isOk $ map (take 4 . iterate (+1)) [2..]