import Data.List hiding (union)
import Data.List.Ordered (union, minus)
import Data.Ord
import Data.Maybe

import Control.Monad

-- Boring prime stuff

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] . map mults . tail $ primes
    mults p = iterate (+ (2 * p)) $ p * p
    union' (x:xs) ys = x: union xs ys

isPrime n = all ((/= 0) . (mod n)) . takeWhile (\p -> p * p <= n) $ primes

-- Calculate helpful boundaries

limit = 10^6

myPrimes = takeWhile (< (div limit 21)) $ primes

maxLen = length . takeWhile (<= limit) . scanl1 (+) $ primes

-- Iterate from maxLen, find the first length that satisfies s

findLocal ps n | length ps < n = Nothing
			   | s > limit = Nothing
			   | isPrime s = Just s
			   | otherwise = findLocal (tail ps) n
			   where s = sum . take n $ ps

findGlobal ps = msum . map (findLocal ps) . iterate (\n -> n - 1) $ maxLen

main = print . fromJust . findGlobal $ myPrimes
