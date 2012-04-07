import Data.Array
import Data.List hiding (union)
import Data.List.Ordered (union, minus)

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr union' [] $ map mults $ tail primes
    mults p = iterate (+ (2 * p)) $ p * p
    union' (x:xs) ys = x: union xs ys

limit = 10^6

primeArr = listArray (0, limit) (repeat False) //
           zip (takeWhile (< limit) primes) (repeat True)

isPrime n | n <= limit = primeArr ! n
          | otherwise = all ((/= 0) . (mod n)) $ takeWhile (<= m) primes
          where m = round $ sqrt $ fromIntegral n

splitAll xs = map ((flip splitAt) xs) [1..(length xs - 1)]

isTruncatable :: Int -> Bool
isTruncatable = all f . splitAll . show where
    f (x, y) = (isPrime (read x)) && (isPrime (read y))

main = print $ sum $ take 11 $ filter isTruncatable $ drop 4 primes
