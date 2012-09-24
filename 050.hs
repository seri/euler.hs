import Data.List hiding (union)
import Data.List.Ordered (union, minus)
import Data.Ord

limit = 10^6

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] $ map mults $ tail primes
    mults p = iterate (+ (2 * p)) (p * p)
    union' (x:xs) ys = x: union xs ys

isPrime n = all ((/= 0) . (mod n)) . takeWhile (\p -> p * p <= n) $ primes

ranged = takeWhile (< (div limit 21)) primes

scanRemove (xsum, xs) = zip (scanl (-) xsum xs) (tails xs)

findLeft = convert . head . filter test . scanRemove where
    test (xsum, xs) = xsum < limit && isPrime xsum
    convert (xsum, xs) = (xsum, length xs)

findRight (xsum, xs) = maximumBy (comparing snd) . map findLeft . 
                       scanRemove $ (xsum, reverse xs)

main = print $ findRight (sum ranged, ranged)