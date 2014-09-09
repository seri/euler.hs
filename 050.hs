import Data.Ord
import Data.Maybe
import Data.List
import qualified Data.List.Ordered as OL
import Control.Monad

allPrimes :: Integral a => [a]
allPrimes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) allPrimes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

isPrime :: Int -> Bool
isPrime n = (all ((/= 0) . (mod n)) . takeWhile (<= sqrt' n)) allPrimes where
    sqrt' = round . sqrt . fromIntegral

limit :: Int
limit = 10^6

primes :: [Int]
primes = (takeWhile (< limit `div` 21)) allPrimes

maxLen :: Int
maxLen = (length . takeWhile (<= limit) . scanl1 (+))  allPrimes

findLocal :: [Int] -> Int -> Maybe Int
findLocal ps n | length ps < n = Nothing
               | s > limit = Nothing
               | isPrime s = Just s
               | otherwise = findLocal (tail ps) n
    where s = sum (take n ps)

findGlobal :: [Int] -> Maybe Int
findGlobal ps = (msum . map (findLocal ps) . iterate decrement) maxLen where
    decrement n = n - 1

main :: IO ()
main = (print . findGlobal) primes