main :: IO ()
main = (print . solve) 100 where
    solve n = sqrSum n - sumSqr n
    sqrSum n = (n * (n + 1) `div` 2) ^ 2
    sumSqr n = n * (n + 1) * (2 * n + 1) `div` 6