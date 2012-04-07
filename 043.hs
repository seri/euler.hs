import Data.List

range = [0..9]

toNumber = foldl (\x y -> 10 * x + y) 0

main = print $ sum [ toNumber [d1, d2, d3, d4, d5, d6, d7, d8, d9, d10]
                     | d2 <- range
                     , d3 <- range \\ [d2]
                     , d4 <- range \\ [d2, d3]
                     , mod (toNumber [d2, d3, d4]) 2 == 0
                     , d5 <- range \\ [d2, d3, d4]
                     , mod (toNumber [d3, d4, d5]) 3 == 0
                     , d6 <- range \\ [d2, d3, d4, d5]
                     , mod (toNumber [d4, d5, d6]) 5 == 0
                     , d7 <- range \\ [d2, d3, d4, d5, d6]
                     , mod (toNumber [d5, d6, d7]) 7 == 0
                     , d8 <- range \\ [d2, d3, d4, d5, d6, d7]
                     , mod (toNumber [d6, d7, d8]) 11 == 0
                     , d9 <- range \\ [d2, d3, d4, d5, d6, d7, d8]
                     , mod (toNumber [d7, d8, d9]) 13 == 0
                     , d10 <- range \\ [d2, d3, d4, d5, d6, d7, d8, d9]
                     , mod (toNumber [d8, d9, d10]) 17 == 0
                     , d1 <- range \\ [d2, d3, d4, d5, d6, d7, d8, d9, d10] ]