import Data.List.Ordered (minus, union)

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] (map mults (tail primes))
    mults p = iterate (+ (2 * p)) (p * p)
    union' (q:qs) xs = q : union qs xs

solve = sum $ takeWhile (<20) primes

main = print solve
