import Data.List
import Data.Maybe
import Control.Monad

isPentagon :: Int -> Bool
isPentagon n = x * x == y && (x + 1) `mod` 6 == 0 where 
    y = 24 * n + 1
    x = (round . sqrt . fromIntegral) y

pentagons :: [Int]
pentagons = scanl (+) 1 diffs where diffs = iterate (+ 3) 4

check :: [Int] -> Maybe Int
check ps = listToMaybe [ d | q <- init ps
                           , let d = p - q
                           , isPentagon d
                           , isPentagon (p + q) ] where
    p = last ps

main :: IO ()
main = (print . msum . map check . drop 3 . inits) pentagons