import Data.Array
import Data.List
import Data.Char

factorial = listArray (0, 9) (1 : scanl1 (*) [1..])

isCurious n = sum (map ((factorial!) . digitToInt) (show n)) == n

curious = filter isCurious [10..(7 * factorial!9)]

main = print $ sum curious