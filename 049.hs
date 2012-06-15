import Data.List
import Data.Maybe

-- The usual prime stuffs

sieve (x:xs) = x : sieve (filter ((/=0) . (`mod` x)) xs)

primes = takeWhile (<10000) . dropWhile (<1000) $ sieve [2..]

-- Given a list of nums, group those which share the same digits

toDigits = sort . map (`mod` 10) . takeWhile (>0) . iterate (`div` 10)

compareBy f x y = compare (f x) (f y)

groupSort f = groupBy (\x y -> f x y == EQ) . sortBy f

groupByDigits = groupSort (compareBy toDigits)

-- Extract an arithmetic sequence of 3 elems out of a list

getArith xs = find (\(x, y, z) -> x + z == y + y)
              [ (x, y, z) | x <- xs , y <- xs, y > x , z <- xs, z > y ]

-- The rest is history

result = map (\(x, y, z) -> show x ++ show y ++ show z) . map fromJust . 
         filter isJust . map getArith . groupByDigits $ primes

main = print result
