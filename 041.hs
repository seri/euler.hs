import Data.Array
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

toNumber :: [Int] -> Int
toNumber = foldl f 0 where f x y = 10 * x + y

main :: IO ()
main = (print . head . filter isPrime . sortBy (flip compare) . map toNumber) 
    ((permutations [1 .. 4]) ++ (permutations [1 .. 7]))