import Data.Digits

main :: IO ()
main = (print . maximum) [ sum (digits 10 (a ^ b)) | a <- domain
                                                   , b <- domain ] where
    domain = [1 .. 99]