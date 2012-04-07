-- aa + bb = cc
-- a + b + c = p
-- => aa + bb = (p - a - b)^2 = pp + aa + bb - 2pa - 2pb + 2ab
-- => pp - 2pa = 2pb - 2ab
-- => 2b(p - a) = p(p - 2a)
-- => b = p(p - 2a) / 2(p - a)
-- Also a <= b < c => a < p/3

import Data.List
import Data.Ord

findB p a = (r == 0) && (b >= a) where
    (b, r) = divMod (p*(p - 2*a)) (2*(p - a))

countSols p = length $ filter (findB p) [1..(div p 3)]

zipBy f xs = zip xs $ map f xs

main = print $ fst $ maximumBy (comparing snd) $ zipBy countSols [12..1000]