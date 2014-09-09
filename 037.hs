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

isTruncatable :: Int -> Bool
isTruncatable = all isCool . splitAll . show where
    isCool (x, y) = (isPrime (read x)) && (isPrime (read y))
    splitAll xs = map ((flip splitAt) xs) [1 .. (length xs - 1)]

main :: IO ()
main = (print . sum . take 11 . filter isTruncatable . drop 4) primes