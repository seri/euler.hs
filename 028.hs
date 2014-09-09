makeSpiral :: Int -> [Int]
makeSpiral n = (take 4 . iterate f) (n * n) where
    f x = x - (n - 1)

sumSpiral :: Int -> Int
sumSpiral n = 1 + sum (concatMap makeSpiral [3, 5 .. n])

main :: IO ()
main = print (sumSpiral 1001)