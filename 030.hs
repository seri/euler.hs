import Data.Char

isSum n = n == (sum $ map ((^5) . digitToInt) $ show n)

main = print $ sum $ filter isSum [10..((9^5)*6)]