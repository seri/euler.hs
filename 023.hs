import Data.List
import Data.Array

primes = 2 : filter (null . tail . pfactors) [3,5..]

pfactors = f primes
    where f ps@(p:pr) n | p*p > n   = [n]
                        | r == 0    = p : f ps q
                        | otherwise = f pr n
                        where (q, r) = divMod n p

aliquot 1 = 0 
aliquot n = (product . map f . group . pfactors $ n) - n where 
    f ps = div (p ^ (length ps + 1) - 1) (p - 1) where p = head ps

abundant n = aliquot n > n

lim = 28123

abundArray = listArray (1, lim) . map abundant $ [1..lim]

abundList = filter (abundArray !) [1..lim]

bad n = any (abundArray !) . map (n -) . takeWhile (< n) $ abundList

main = print . sum . filter (not . bad) $ [1..lim]