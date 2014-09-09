import Data.List
import Data.Digits

identical :: Eq a => [a] -> Bool
identical xs = if null xs then False else all (== head xs) (tail xs)

isCool :: Int -> Bool
isCool n = (identical . map (sort . digits 10) . take 6 . iterate (+ n)) n

main :: IO ()
main = (print . find isCool . iterate (+ 1)) 10000