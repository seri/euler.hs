import Data.Char

main :: IO ()
main = (print . sum . map digitToInt . show . product) [1 .. 100]