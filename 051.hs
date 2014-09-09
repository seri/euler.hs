import Data.List
import qualified Data.List.Ordered as OL
import Data.Maybe
import Data.Array
import Data.Digits
import Control.Monad

allPrimes :: Integral a => [a]
allPrimes = 2: 3: OL.minus [5, 7..] sieve where
    sieve = (foldr safeUnion [] . map skipList . tail) allPrimes
    skipList p = iterate (+ (p + p)) (p * p)
    safeUnion (x:xs) ys = x : OL.union xs ys

domain :: (Int, Int)
domain = (56993, 999999)

primes :: [Int]
primes = (takeWhile (<= snd domain) . dropWhile (<= fst domain)) allPrimes

isPrime :: Int -> Bool
isPrime = (arr !) where
    arr = listArray (2, snd domain) (repeat False) // 
          zip primes (repeat True)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

replaceAll :: Eq a => a -> [a] -> a -> [a]
replaceAll x ys z = map (f x z) ys where
    f x z y = if y == x then z else y

findMaskDigit :: [Int] -> Maybe Int
findMaskDigit xs = (msum . map (try xs)) [0, 1, 2] where 
    try xs x = if count x xs == 3 then Just x
                                  else Nothing

findMask :: [Int] -> Maybe [Int]
findMask xs = if isJust d then Just (replaceAll (fromJust d) xs (-1))
                          else Nothing
    where d = findMaskDigit xs

generateFamily :: Int -> [Int]
generateFamily p = if isJust mask then result else [] where 
    mask = (findMask . (digits 10)) p
    result = map ((unDigits 10) . (replaceAll (-1) (fromJust mask))) [0..9]

isFamilyCool :: [Int] -> Bool
isFamilyCool ps = count True (map isPrime ps) == 8

main :: IO ()
main = (print . msum . map try) primes where
    try p = if isFamilyCool (generateFamily p) then Just p
                                               else Nothing

{-
    Some observations:
    - The last digit can only be 1, 3, 7, 9 so it doesn't belong to the mask
    - The mask has a size of 3x. We will cheat and assume that it is indeed 3
    - The smallest prime of the family will have a mask of 000, 111, or 222
    - We will cheat again and assume that the family has no more than 6 digits
    - The last cheat: all remaining digits of the answer != the repeated digit
-}