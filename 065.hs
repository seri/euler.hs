import Data.Digits
import Data.Ratio

type Irrational = (Integer, [Integer])

approx :: Irrational -> Int -> Rational
approx (x, xs) n = x % 1 + 1 / foldr f 0 (take (n - 1) xs) where
    f x 0 = x % 1
    f x r = x % 1 + (1 / r)

euler :: Irrational
euler = (2, concatMap surround (iterate (+ 2) 2)) where
    surround x = [1, x, 1]

main :: IO ()
main = (print . sum . digits 10 . numerator) (euler `approx` 100)