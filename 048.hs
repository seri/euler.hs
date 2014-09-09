import Data.List

powMod :: Integer -> Integer -> Integer -> Integer
powMod m x 0 = 1
powMod m x 1 = x `mod` m
powMod m x n = (((go p) ^ 2) * (go q)) `mod` m where
    (p, q) = n `divMod` 2
    go = powMod m x

sumMod :: Integer -> Integer -> Integer -> Integer
sumMod m x y = (x + y) `mod` m

modula :: Integer
modula = 10^10

main :: IO ()
main = (print . foldl1 (sumMod modula) . map f) [1..1000] where
    f x = powMod modula x x