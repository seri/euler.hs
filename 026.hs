import Data.List
import Data.Ord

simplify :: Integer -> Integer
simplify n | n `mod` 2 == 0 = simplify (n `div` 2)
           | n `mod` 5 == 0 = simplify (n `div` 5)
           | otherwise = n

nines :: [Integer]
nines = map (flip (-) 1) (iterate (* 10) 10)

cyclen :: Integer -> Int
cyclen n = ((+ 1) . length . takeWhile ((/= 0) . (flip mod (simplify n)))) nines

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f xs = (zip xs . map f) xs

main :: IO ()
main = (print . fst . maximumBy (comparing snd) . zipMap cyclen) [3 .. 999]