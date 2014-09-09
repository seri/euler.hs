hexagonals :: [Int]
hexagonals = scanl (+) 1 diffs where diffs = iterate (+ 4) 5

isPentagon :: Int -> Bool
isPentagon n = x * x == y && (x + 1) `mod` 6 == 0 where 
    y = 24 * n + 1
    x = (round . sqrt . fromIntegral) y

main :: IO ()
main = (print . head . filter isPentagon . drop 143) hexagonals