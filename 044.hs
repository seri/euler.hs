import Data.List

isPent n = x * x == y && mod (x + 1) 6 == 0 where 
    y = 24*n + 1
    x = round $ sqrt $ fromIntegral y

pents = scanl (+) 1 diffs where
    diffs = iterate (+ 3) 4

check ps = [ d | q <- init ps
               , let d = p - q
               , isPent d
               , isPent (p + q) ] where p = last ps

main = print $ head $ head $ filter (not . null) $ 
               map check $ drop 3 $ inits pents