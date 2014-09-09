import Data.Digits

isSum :: Int -> Bool
isSum n = ((== n) . sum . map (^ 5) . digits 10) n

main :: IO ()
main = (print . sum . filter isSum) [10 .. ((9 ^ 5) * 6)]