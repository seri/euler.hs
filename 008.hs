import Data.Char

largest :: [Int] -> Int
largest (x1:(xs@(x2:x3:x4:x5:_))) = 
    max (product [x1, x2, x3, x4, x5]) (largest xs)
largest _ = 0

solve :: String -> Int
solve = largest . map digitToInt . filter isDigit

main = readFile "008.in" >>= print . solve