import Data.List

main = (print . length . nub) [ a ^ b | a <- domain, b <- domain ] where
    domain = [2 .. 100]