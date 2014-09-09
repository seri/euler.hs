import Data.Digits

main = (print . sum . filter isCurious) [10 .. (7 * factorial 9)] where
    isCurious n = ((== n) . sum . map factorial . digits 10) n
    factorial x = product [1 .. x]