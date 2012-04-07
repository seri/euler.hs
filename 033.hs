curious = [ (round p, round q) | a <- [1..8]
                               , b <- [(a + 1)..9]
                               , c <- [(a + 1)..9]
                               , let p = 10 * a + b
                               , let q = 10 * b + c 
                               , a/c == p/q ]

productPair xs = (product ps, product qs) where (ps, qs) = unzip xs

simplify (p, q) = (div p x, div q x) where x = gcd p q

main = print $ snd $ simplify $ productPair curious