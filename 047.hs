import Data.List
import qualified Data.List.Ordered as OL

primes :: Integral a => [a]
primes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) primes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

factorize :: [Int] -> Int -> [Int]
factorize ps@(p:pr) n | n <= 1 = []
                      | p * p > n = [n]
                      | r == 0 = p : factorize ps q
                      | otherwise = factorize pr n
                      where (q, r) = n `divMod` p

isCool :: [Int] -> Bool
isCool = all ((== 4) . length . group . factorize primes)

main :: IO ()
main = (print . head . head . filter isCool . map (take 4 . iterate (+ 1))) [2 ..]