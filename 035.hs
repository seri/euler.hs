import Data.Array
import Data.List
import qualified Data.List.Ordered as OL

allPrimes :: Integral a => [a]
allPrimes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) allPrimes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

limit :: Int
limit = 10 ^ 6

primes :: [Int]
primes = takeWhile (< limit) allPrimes

isPrime :: Int -> Bool
isPrime = (arr !) where
    arr = listArray (0, limit) (repeat False) //
          zip primes (repeat True)

isCircular :: Int -> Bool
isCircular = all (isPrime . read) . init . rotations . show where
    rotations xs = map (rotate xs) [1 .. length xs]
    rotate xs n = right ++ left where (left, right) = splitAt n xs

main :: IO ()
main = (print . length . filter isCircular) primes