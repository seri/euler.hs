import Data.Char

list = concat $ map show [0..]

digit = digitToInt . (list !!)

main = print $ product $ map digit $ take 7 $ iterate (*10) 1