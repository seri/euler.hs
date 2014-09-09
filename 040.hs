import Data.Char

digit :: Int -> Int
digit = digitToInt . (list !!) where list = concatMap show [0 ..]

main :: IO ()
main = (print . product . map digit . take 7 . iterate (* 10)) 1