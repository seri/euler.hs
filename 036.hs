import Numeric
import Data.Char

palin xs = xs == reverse xs

palinBase base n = palin $ showIntAtBase base intToDigit n ""

satisfy n = palinBase 10 n && palinBase 2 n

main = print $ sum $ filter satisfy [1..999999]