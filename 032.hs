import Data.List
import Data.Char

check :: Show a => [a]-> Bool
check = (== "123456789") . sort . concatMap show 

main :: IO ()
main = (print . sum . nub) [ z | x <- [2 .. 98]
                               , y <- [123 .. (9999 `div` x)]
                               , let z = x * y
                               , check [x, y, z] ]