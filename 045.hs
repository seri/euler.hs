hexes = scanl (+) 1 diffs where
    diffs = iterate (+ 4) 5

isPent n = x * x == y && mod (x + 1) 6 == 0 where
    y = 24 * n + 1
    x = round $ sqrt $ fromIntegral y

main = print $ head $ filter isPent $ drop 143 hexes