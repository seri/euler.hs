import Data.Ord
import Data.List
import Data.Digits
import Data.Maybe
import Control.Monad

sieve :: [Int] -> [Int]
sieve (x:xs) = x : sieve (filter ((/=0) . (`mod` x)) xs)

primes :: [Int]
primes = (takeWhile (< 10000) . dropWhile (< 1000) . sieve) [2 ..]

groupSort :: (a -> a -> Ordering) -> [a] -> [[a]] 
groupSort f = groupBy g . sortBy f where g x y = f x y == EQ

groupByDigits :: [Int] -> [[Int]]
groupByDigits = groupSort (comparing toDigits) where
    toDigits = sort . (digits 10)

isHomogeneous :: Eq a => [a] -> Bool
isHomogeneous xs = if null xs then False else all (== head xs) (tail xs)

isArith :: [Int] -> Bool
isArith xs = isHomogeneous (zipWith (-) (tail xs) (init xs))

findArith :: [Int] -> Maybe [Int]
findArith xs = find isArith [ [x, y, z] | x <- xs
                                        , x /= 1487
                                        , y <- xs, y > x 
                                        , z <- xs, z > y ]

solve :: Maybe String
solve = fmap (concatMap show) ((msum . map findArith . groupByDigits) primes)

main :: IO ()
main = print solve