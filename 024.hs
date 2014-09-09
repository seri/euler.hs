import Data.List

factorials :: [Int]
factorials = 1 : scanl1 (*) [1 ..]

nthPermute :: String -> Int -> String
nthPermute xs n = if null xs then [] 
                             else x : nthPermute (delete x xs) q where
    f = factorials !! (length xs - 1)
    (p, q) = n `divMod` f
    x = xs !! p

main :: IO ()
main = (print . nthPermute "0123456789") 999999