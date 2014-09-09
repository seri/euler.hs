import Data.List
import qualified Data.List.Ordered as OL

primes :: Integral a => [a]
primes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) primes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

isPrime :: Int -> Bool
isPrime n = (all ((/= 0) . (mod n)) . takeWhile (<= sqrt' n)) primes where
    sqrt' = round . sqrt . fromIntegral

countPrimes :: [Int] -> Int
countPrimes = length . filter isPrime

nextCycle :: [Int] -> [Int]
nextCycle [x1, x2, x3, x4] = (tail . take 5 . iterate (+ d)) x4 where 
    d = x4 - x3 + 2

spiral :: [[Int]]
spiral = iterate nextCycle [3, 5, 7, 9]

countCycles :: Int
countCycles = (length . tail . takeWhile isUncool . scanl f (0, 0)) spiral where
    isUncool (n, p) = p * 10 >= n
    f (n, p) xs = (n + 4, p + countPrimes xs)

main :: IO ()
main = print sideLength where sideLength = 1 + 2 * countCycles

-- TODO: optimize