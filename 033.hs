type Pair = (Int, Int)

curious :: [Pair]
curious = [ (p,  q) | x <- [1 .. 8]
                    , let rest = [(x + 1) .. 9]
                    , y <- rest
                    , z <- rest
                    , let p = 10 * x + y
                    , let q = 10 * y + z 
                    , x * q == p * z ]

productPair :: [Pair] -> Pair
productPair xs = (product ps, product qs) where (ps, qs) = unzip xs

simplify :: Pair -> Pair
simplify (p, q) = (p `div` x, q `div` x) where x = gcd p q

main :: IO ()
main = (print . snd . simplify . productPair) curious