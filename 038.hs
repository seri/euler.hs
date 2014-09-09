import Data.List

main :: IO ()
main = (print . maximum)
    [ n | a <- domain
        , b <- domain \\ [a]
        , c <- domain \\ [a, b]
        , let x = 9000 + 100 * a + 10 * b + c
        , let y = x * 2
        , let z = show x ++ show y
        , sort z == "123456789"
        , let n = read z :: Int ]
    where domain = [1 .. 8]