import Data.List

count :: Int -> [Int] -> Int
count n a | n < 0 = 0
          | n == 0 = 1
          | null a = 0
          | otherwise = count n (tail a) + count (n - head a) a

main :: IO ()
main = (print . count 200 . reverse) [1, 2, 5, 10, 20, 50, 100, 200]