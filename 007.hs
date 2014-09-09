primes :: [Int]
primes = 2 : filter ((== 1) . length . primeFactors) [3, 5..]

primeFactors :: Int -> [Int]
primeFactors n = factors n primes where
    factors n (p:ps) | p * p > n = [n]
                     | n `mod` p == 0 = p : factors (n `div` p) ps
                     | otherwise = factors n ps

main :: IO ()
main = print (primes !! 10000)