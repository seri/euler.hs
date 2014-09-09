import Data.List
import Data.Ord

findB :: Int -> Int -> Bool
findB p a = r == 0 && b >= a where
    (b, r) = (p * (p - 2 * a)) `divMod` (2 * (p - a))

countSols :: Int -> Int
countSols p = (length . filter (findB p)) [1.. (p `div` 3)]

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f xs = zip xs (map f xs)

main :: IO ()
main = (print . fst . maximumBy (comparing snd) . zipMap countSols) [12 .. 1000]

-- Explanation:
-- aa + bb = cc
-- a + b + c = p
-- => aa + bb = (p - a - b)^2 = pp + aa + bb - 2pa - 2pb + 2ab
-- => pp - 2pa = 2pb - 2ab
-- => 2b(p - a) = p(p - 2a)
-- => b = p(p - 2a) / 2(p - a)
-- Also a <= b < c => a < p/3