evenFibs :: [Int]
evenFibs = 2 : 8 : zipWith next evenFibs (tail evenFibs) where
    next x y = x + 4 * y

main :: IO ()
main = (print . sum . takeWhile (<= 4000000)) evenFibs