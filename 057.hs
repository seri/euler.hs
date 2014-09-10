import Data.Digits
import Data.Ratio

series :: [Rational]
series = iterate next (1 + (1 % 2)) where
    next x = 1 + (1 / (1 + x))

isCool :: Rational -> Bool
isCool x = f (numerator x) > f (denominator x) where 
    f :: Integer -> Int
    f n = (length . digits 10) n

main :: IO ()
main = (print . length . filter isCool . take 1000) series