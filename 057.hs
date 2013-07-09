import Data.Digits
import Data.Ratio

inverse x = (denominator x) % (numerator x)

series = iterate (\x -> 1 + (inverse (1 + x))) (1 + (1 % 2))

check x = f (numerator x) > f (denominator x)
    where f n = length . digits 10 $ n

main = print $ length . filter check . take 1000 $ series