import Data.Ord
import Data.List
import qualified Data.List.Ordered as OL

primes :: Integral a => [a]
primes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) primes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

isPrime :: Integral a => a -> Bool
isPrime n | n > 1 = (all ((/= 0) . (mod n)) . takeWhile (<= sqrt' n)) primes 
          | otherwise = False 
    where sqrt' = ceiling . sqrt . fromIntegral

primeLen :: Int -> Int -> Int
primeLen x y = (length . takeWhile isPrime . map f) [0 ..] where
    f n = n * n + x * n + y

main :: IO ()
main = (print . fst . maximumBy (comparing snd))
    [ (x * y, primeLen x y ) | x <- domain
                             , y <- domain
                             , isPrime y ] where
    domain = [-999 .. 999]
