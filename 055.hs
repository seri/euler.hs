import Data.Digits
import Data.List

limit :: Int
limit = 10000

palindrome :: Int -> Bool
palindrome n = ds == reverse ds where ds = digits 10 n

compute :: Int -> Int
compute n = n + unDigits 10 (reverse (digits 10 n))

isLychrel :: Int -> Bool
isLychrel = not . any palindrome . take 50 . tail . iterate compute

main :: IO ()
main = (print . length . filter isLychrel) [1 .. limit]