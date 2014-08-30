evenFibs = 2 : 8 : zipWith (\x y -> x + 4*y) evenFibs (tail evenFibs)
main = print . sum . takeWhile (<= 4000000) $ evenFibs