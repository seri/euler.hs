import Data.List
import Data.Char

check a b c = sort (show a ++ show b ++ show c) == "123456789"

main = print $ sum $ nub [ c | a <- [2..98]
                             , b <- [123..(div 9999 a)]
                             , let c = a * b
                             , check a b c ]
