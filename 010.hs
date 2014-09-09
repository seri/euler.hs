import qualified Data.List.Ordered as OL

-- A faster prime sieve
-- ====================

primes :: Integral a => [a]
primes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) primes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

main :: IO ()
main = (print . sum . takeWhile (< 2000000)) primes

-- Read more:
-- http://www.haskell.org/haskellwiki/Prime_numbers#Sieve_of_Eratosthenes