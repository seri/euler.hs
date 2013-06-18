pascal = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]

main = print $ length . filter (> 10^6) . concat . take 101 $ pascal
