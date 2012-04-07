import Data.List

primes = 2 : filter ((== 1) . length . primeFactors) [ 3, 5.. ]

primeFactors = factors primes

factors (p:ps) n | p*p > n = [n]
                 | mod n p == 0 = p : factors (p:ps) (div n p)
                 | otherwise = factors ps n

tau = product . map ((+ 1) . length) . group . primeFactors

solve = head . filter ((> 500) . tau) . scanl1 (+) $ [ 1.. ]

main = print solve
