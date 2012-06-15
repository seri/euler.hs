import Data.List

powMod m x 0 = 1
powMod m x 1 = mod x m
powMod m x n = mod (((recur p) ^ 2) * (recur q)) m where
    recur = powMod m x
    (p, q) = divMod n 2

sumMod m x y = mod (x + y) m

modula = 10^10

result = foldl1 (sumMod modula) $ map (\x -> powMod modula x x) [1..1000]

main = print result