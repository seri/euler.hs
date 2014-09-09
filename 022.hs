import Data.List
import Data.Char

solve :: String -> Int
solve input = (sum . zipWith score [1 ..]) names where
    names = (sort . read) ("[" ++ input ++ "]")
    score i name = i * ((sum . map alpha) name)
    alpha c = ord c - ord 'A' + 1

main :: IO ()
main = print . solve =<< readFile "022.in"