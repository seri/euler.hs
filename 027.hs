{-# OPTIONS_GHC -O2 #-}

import Data.List (maximumBy)
import Data.Ord
import Data.List.Ordered (minus, union)

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] (map mults (tail primes))
    mults p = iterate (+ (2 * p)) (p * p)
    union' (x:xs) ys = x: union xs ys

isPrime n | n <= 1 = False
          | otherwise = all ((/=0) . mod n) (takeWhile (\p -> p*p <= n) primes)

primeLen a b = length . takeWhile isPrime . map formula $ [0..] where
    formula n = n*n + a*n + b

main = print $ fst . maximumBy (comparing snd) $
               [ (a*b, primeLen a b ) | a <- range
                                      , b <- range
                                      , isPrime b ] where
    range = [-999..999]
