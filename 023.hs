import Data.List
import Data.Array

primes :: [Int]
primes = 2 : filter (null . tail . primeFactors) [3, 5 ..]

primeFactors :: Int -> [Int]
primeFactors = factors primes where 
    factors ps@(p:pr) n | p * p > n = [n]
                        | r == 0 = p : factors ps q
                        | otherwise = factors pr n
                        where (q, r) = n `divMod` p

aliquot :: Int -> Int
aliquot 1 = 0
aliquot n = ((product . map f . group . primeFactors) n) - n where 
    f ps@(p:pr) = (p * product ps - 1) `div` (p - 1)

numbers :: [Int]
numbers = [1 .. 28123]

isAbundant :: Int -> Bool
isAbundant = (memo !) where
    memo = (listArray (1, 28123) . map f) numbers
    f n = aliquot n > n

abundants :: [Int]
abundants = filter isAbundant numbers

isBad :: Int -> Bool
isBad n = (any isAbundant . map (n -) . takeWhile (< n)) abundants

main :: IO ()
main = (print . sum . filter (not . isBad)) numbers