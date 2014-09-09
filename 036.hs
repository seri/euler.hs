import Numeric
import Data.Char

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

isCool :: Int -> Int -> Bool
isCool base n = isPalindrome (showIntAtBase base intToDigit n "")

satisfy :: Int -> Bool
satisfy n = isCool 10 n && isCool 2 n

main :: IO ()
main = (print . sum . filter satisfy) [1 .. 999999]