import Data.List

primes :: [Int]
primes = 2 : filter ((== 1) . length . primeFactors) [ 3, 5.. ]

primeFactors :: Int -> [Int]
primeFactors = factors primes where
    factors (p:ps) n | p * p > n = [n]
                     | n `mod` p == 0 = p : factors (p:ps) (n `div` p)
                     | otherwise = factors ps n

tau :: Int -> Int
tau = product . map ((+ 1) . length) . group . primeFactors

solve :: Int
solve = (head . filter ((> 500) . tau) . scanl1 (+)) [1.. ]

main :: IO ()
main = print solve