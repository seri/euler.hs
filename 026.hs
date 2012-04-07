-- Reading: http://www.lrz.de/~hr/numb/period.html

import Data.List
import Data.Ord

simplify n | mod n 2 == 0 = simplify $ div n 2
           | mod n 5 == 0 = simplify $ div n 5
           | otherwise = n

nines = map (flip (-) 1) . iterate (*10) $ 10

cyclen n = (+1) . length . takeWhile ((/=0) . (flip mod $ simplify n)) $ nines

zipMap f xs = zip xs . map f $ xs

main = print $ fst . maximumBy (comparing snd) . zipMap cyclen $ [3..999]