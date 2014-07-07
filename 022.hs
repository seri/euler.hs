import Data.List
import Data.Char

solve names = sum . zipWith score [1..] . sort . read $ "[" ++ names ++ "]"
    where score i name = i * (sum . map alpha $ name)
          alpha c = ord c - ord 'A' + 1

main = print . solve =<< readFile "022.in"

