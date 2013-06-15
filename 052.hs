import Data.List
import Data.Digits

isSame xs = xs == replicate (length xs) (head xs) 

isCool n = isSame . map (sort . (digits 10) . (* n)) $ [1..6]

search = head . filter isCool . iterate (+3) $ 100002

main = print search
