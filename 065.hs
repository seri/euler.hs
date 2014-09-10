import Data.Digits
import Data.Ratio

type Fancy = (Integer, [Integer])

nthConverge :: Fancy -> Int -> Rational
nthConverge (x, xs) n = x % 1 + 1 / reduce (take (n - 1) xs) where
    reduce = foldr f (0 % 1)
    f x 0 = x % 1
    f x r = x % 1 + (1 / r)

fancyE :: Fancy
fancyE = (2, concatMap surround (iterate (+ 2) 2)) where
    surround x = [1, x, 1]

main :: IO ()
main = (print . sum . digits 10 . numerator) (fancyE `nthConverge` 100 )