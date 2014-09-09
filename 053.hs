pascal :: Integral a => [[a]]
pascal = iterate nextRow [1, 1] where
    nextRow row = zipWith (+) (0 : row) (row ++ [0])

main :: IO ()
main = (print . length . filter (> 10 ^ 6) . concat . take 100) pascal