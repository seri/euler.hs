import Data.List

factorials = 1 : scanl1 (*) [1..]

nthPermute [] _ = []
nthPermute xs n = x : nthPermute (delete x xs) q where
    f = factorials !! (length xs - 1)
    (p, q) = divMod n f
    x = xs !! p

main = print $ nthPermute "0123456789" 999999
