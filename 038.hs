import Data.List

range = [1..8]

main = print $ [ z | a <- range
                   , b <- range \\ [a]
                   , c <- range \\ [a, b]
                   , let x = 9000 + 100*a + 10*b + c
                   , let y = x * 2
                   , let z = (show x) ++ (show y)
                   , sort z == "123456789" ]