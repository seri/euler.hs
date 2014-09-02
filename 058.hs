-- TODO: optimize

import Data.List hiding (union)
import Data.List.Ordered (union, minus)

-- Prime things here we go again

primes = 2: 3: minus odds sieve where
    odds = iterate (+2) 5
    sieve = foldr safeUnion [] . map skipList . tail $ primes
    skipList p = iterate (+ (2 * p)) $ p * p 
    safeUnion (x:xs) ys = x : union xs ys

isPrime n = all ((/= 0) . (mod n)) $ takeWhile (\p -> p * p <= n) primes

countPrimes = length . filter isPrime

-- Generate the numbers in the diagonals

nextCycle [x1, x2, x3, x4] = tail . take 5 . iterate (+d) $ x4
    where d = (x4 - x3) + 2

spiral = iterate nextCycle [3, 5, 7, 9]

-- Search for the number of cycles needed to reach the required state

search = length . tail . takeWhile isUncool . scanl f (0, 0) $ spiral where
    isUncool (n, p) = p * 10 >= n
    f (n, p) xs = (n + 4, p + countPrimes xs)

sideLength cycleNumber = 1 + 2 * cycleNumber

-- Reap immense benefits

main = print $ sideLength search