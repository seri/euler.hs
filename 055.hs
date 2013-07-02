import Data.Digits
import Data.List

limit = 10000

isPalin n = ds == reverse ds where ds = digits 10 n

backward n = unDigits 10 (reverse ds) where ds = digits 10 n

compute n = n + backward n 

isLychrel = not . any isPalin . take 50 . tail . iterate compute

main = print . length . filter isLychrel $ [1..limit]