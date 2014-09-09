import Data.Char

largest :: [Int] -> Int
largest xs = if length xs < 5 then 0
                              else max (product (take 5 xs)) (largest (tail xs))

solve :: String -> Int
solve = largest . map digitToInt . filter isDigit

main :: IO ()
main = readFile "008.in" >>= print . solve