import Data.List
import Data.Array
import qualified Data.List.Ordered as OL

limit :: Int
limit = 10000

allPrimes :: [Int]
allPrimes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) primes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

primes :: [Int]
primes = takeWhile (< limit) allPrimes

isPrimeCached :: Int -> Bool
isPrimeCached = (arr !) where
    arr = listArray (0, limit) (repeat False) //
          zip primes (repeat True)

isPrimeUncached :: Int -> Bool
isPrimeUncached n = (all ((/= 0) . (mod n)) . takeWhile (<= sqrt' n)) primes where
    sqrt' = round . sqrt . fromIntegral

isPrime :: Int -> Bool    
isPrime n = if n <= limit then isPrimeCached n
                          else isPrimeUncached n

isCool :: Int -> Int -> Bool
isCool p q = isPrime (combine p q) && isPrime (combine q p) where
    combine x y = read (show x ++ show y) :: Int

solve :: Int
solve = head xs where
    xs = [ sum [a, b, c, d, e] 
         | a <- primes
         , b <- dropWhile (<= a) primes
         , isCool a b
         , c <- dropWhile (<= b) primes
         , isCool a c && isCool b c
         , d <- dropWhile (<= c) primes
         , isCool a d && isCool b d && isCool c d
         , e <- dropWhile (<= d) primes
         , isCool a e && isCool b e && isCool c e && isCool d e ]

main :: IO ()
main = print solve

-- TODO: optimize and/or prettify