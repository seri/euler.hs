import Data.List
import qualified Data.List.Ordered as OL

primes :: Integral a => [a]
primes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) primes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

nonSquared :: Int -> Bool
nonSquared n = m * m /= n where m = (round . sqrt . fromIntegral) n

isCool :: Int -> Bool
isCool n = (all nonSquared . map f . takeWhile (<= n)) primes where
    f p = (n - p) `div` 2

main :: IO ()
main = (print . head . filter isCool) [9, 11 ..]