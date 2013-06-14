{-
    Some observations:
    - The last digit can only be 1, 3, 7, 9 so it doesn't belong to the mask.
    - The mask has a size of 3x. We will cheat and assume that it is indeed 3.
    - The smallest prime of the family will have a mask of 000, 111, or 222.
    - We will cheat again and assume that family has no more than 6 digits.
    - The last cheat: all remaining digits of the answer != the repeated digit
-}

import Data.List hiding (union)
import Data.List.Ordered (union, minus)
import Data.Array
import Data.Digits

import Control.Monad
import Data.Maybe


-- Prime meme

upperBound = 999999
lowerBound = 56993

primes = 2: 3: minus [5, 7..] sieve where
    sieve = foldr safeUnion [] . map skipList . tail $ primes
    skipList p = iterate (+ (2 * p)) $ p * p
    safeUnion (x:xs) ys = x: union xs ys

myPrimes = takeWhile (<= upperBound) . dropWhile (<= lowerBound) $ primes

primeArr = listArray (2, upperBound) (repeat False) // 
           zip myPrimes (repeat True)

isPrime n = primeArr ! n


-- Some list utils

count x [] = 0
count x (y:xs) = if x == y then 1 + count x xs
                           else count x xs

replaceAll p [] q = []
replaceAll p (x:xs) q = if x == p then q : replaceAll p xs q
                                  else x : replaceAll p xs q


-- Transform a list of digits into masked form

findMaskDigit xs = msum . map (try xs) $ [0, 1, 2] 
    where try xs x = if count x xs == 3 then Just x
                                        else Nothing

findMask xs = if isJust temp then Just $ replaceAll (fromJust temp) xs (-1)
                             else Nothing
    where temp = findMaskDigit xs


-- Given the smallest member, generate all members of the family

generateFamily p =
    if isJust temp then map ((unDigits 10) . (replaceAll (-1) (fromJust temp))) [0..9]
                   else []
    where temp = (findMask . (digits 10)) p

isFamilyCool ps = count True (map isPrime ps) == 8

 

search = msum . map try $ myPrimes
    where try p = if isFamilyCool (generateFamily p) then Just p
                                                     else Nothing

main = print search
