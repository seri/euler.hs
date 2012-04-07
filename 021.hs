import Data.List

primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors = factors primes
    where factors ps@(p:pr) n | p*p > n = [n]
                              | r == 0 = p : factors ps q
                              | otherwise = factors pr n
                              where (q, r) = divMod n p

aliquot n = (product . map f . group . primeFactors $ n) - n
    where f ps@(p:_) = div (p * product ps - 1) (p - 1)

aliquots = 0 : 1 : map aliquot [2..9999]

solve = sum . filter amicable $ [1..9999]
    where amicable n | m == n    = False
                     | m < 10000 = aliquots!!m == n
                     | otherwise = aliquot m == n
              where m = aliquots!!n

main = print solve
